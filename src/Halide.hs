{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Halide where

import Control.Arrow ((>>>))
import Control.Lens (Lens', lens, (%~))
import Control.Monad.State (State, gets, modify)
import Data.Data (Proxy (..))
import Data.Function (on)
import Data.Generics.Fixplate
import Data.Generics.Labels ()
import Data.String (IsString)
import Data.Text (Text, intercalate, pack)

--------------------------------------------------------------------------------

newtype Var = Var {unVar :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

newtype FuncName = FuncName {unFuncName :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

newtype Param (a :: ScalarType) = Param Text
  deriving (Show)

newtype ImageParam (d :: Dimensions) (a :: ScalarType) = ImageParam Text
  deriving (Show)

data Func (d :: Dimensions) (a :: ScalarType) = Func
  { name :: FuncName,
    body :: InferArgs d -> Expr a
  }

-- All Exprs have a scalar type, and all Funcs evaluate to one or
-- more scalar types. The scalar types in Halide are unsigned
-- integers of various bit widths, signed integers of the same set
-- of bit widths, Fing point numbers in single and double
-- precision, and opaque handles (equivalent to void *). The
-- following array contains all the legal types.
data ScalarType
  = U8
  | U16
  | U32
  | U64
  | F32
  | F64

type Cast a = (Proxy a, Text)

type CastSimple = Text

castToCastSimple :: Cast a -> CastSimple
castToCastSimple = snd

u8 :: Cast 'U8
u8 = (Proxy, "UInt(8)")

u16 :: Cast 'U16
u16 = (Proxy, "UInt(16)")

u32 :: Cast 'U32
u32 = (Proxy, "UInt(32)")

u64 :: Cast 'U64
u64 = (Proxy, "UInt(64)")

f32 :: Cast 'F32
f32 = (Proxy, "Float(32)")

f64 :: Cast 'F64
f64 = (Proxy, "Float(64)")

data Dimensions = D0 | D1 | D2 | D3 | D4

type family InferType a b where
  InferType 'F64 _ = 'F64
  InferType _ 'F64 = 'F64
  InferType 'F32 _ = 'F32
  InferType _ 'F32 = 'F32
  InferType 'U64 _ = 'U64
  InferType _ 'U64 = 'U64
  InferType 'U32 _ = 'U32
  InferType _ 'U32 = 'U32
  InferType 'U16 _ = 'U16
  InferType _ 'U16 = 'U16
  InferType 'U8 _ = 'U8
  InferType _ 'U8 = 'U8
  InferType a a = a

type family InferArgs a where
  InferArgs 'D0 = N0 Var
  InferArgs 'D1 = N1 Var
  InferArgs 'D2 = N2 Var
  InferArgs 'D3 = N3 Var
  InferArgs 'D4 = N4 Var

data Expr (a :: ScalarType) where
  EVar :: Var -> Expr 'U32
  EInt :: Int -> Expr 'U64
  EDouble :: Double -> Expr 'F64
  EAdd :: Expr a -> Expr b -> Expr (InferType a b)
  ESub :: Expr a -> Expr b -> Expr (InferType a b)
  EMul :: Expr a -> Expr b -> Expr (InferType a b)
  EMin :: Expr a -> Expr b -> Expr (InferType a b)
  EMax :: Expr a -> Expr b -> Expr (InferType a b)
  EAt :: At (d :: Dimensions) a -> Expr a
  ECast :: Cast (b :: ScalarType) -> Expr a -> Expr b

instance Num (Expr (a :: ScalarType)) where
  (+) = EAdd
  (-) = ESub
  (*) = EMul

data N0 a = N0

newtype N1 a = N1 a
  deriving (Functor)

data N2 a = N2 a a
  deriving (Functor)

data N3 a = N3 a a a
  deriving (Functor)

data N4 a = N4 a a a a
  deriving (Functor)

class HasCoordinates d where
  type Coordinates d
  coordinatesToList :: Coordinates d -> [Expr 'U32]

instance HasCoordinates 'D0 where
  type Coordinates 'D0 = N0 (Expr 'U32)
  coordinatesToList _ = []

instance HasCoordinates 'D1 where
  type Coordinates 'D1 = N1 (Expr 'U32)
  coordinatesToList (N1 v) = [v]

instance HasCoordinates 'D2 where
  type Coordinates 'D2 = N2 (Expr 'U32)
  coordinatesToList (N2 v1 v2) = [v1, v2]

instance HasCoordinates 'D3 where
  type Coordinates 'D3 = N3 (Expr 'U32)
  coordinatesToList (N3 v1 v2 v3) = [v1, v2, v3]

instance HasCoordinates 'D4 where
  type Coordinates 'D4 = N4 (Expr 'U32)
  coordinatesToList (N4 v1 v2 v3 v4) = [v1, v2, v3, v4]

--------------------------------------------------------------------------------

data At d a = forall f. Space d f => At {space :: f d a, coordinates :: Coordinates d}

data AtSimple = AtSimple {atSpaceID :: Text, atCoordinates :: [Expr 'U32]}

atToAtSimple :: forall d a. (HasCoordinates d) => At d a -> AtSimple
atToAtSimple (At space coordinates) =
  AtSimple
    { atSpaceID = spaceID space,
      atCoordinates = coordinatesToList @d coordinates
    }

data ExprF f
  = FVar Var
  | FInt Int
  | FDouble Double
  | FAdd f f
  | FSub f f
  | FMul f f
  | FMin f f
  | FMax f f
  | FAt Text [f]
  | FCast CastSimple f
  deriving (Functor)

data Tag = Tag
  { vars :: [Var],
    params :: [Text],
    spaces :: [Text]
  }

instance Semigroup Tag where
  t1 <> t2 =
    Tag
      { vars = ((<>) `on` vars) t1 t2,
        params = ((<>) `on` params) t1 t2,
        spaces = ((<>) `on` spaces) t1 t2
      }

instance Monoid Tag where
  mempty = Tag [] [] []

type MuExprF = Mu ExprF

type AST = Attr ExprF Tag

fixAnn :: p -> f (Attr f p) -> Attr f p
fixAnn tag v = Fix $ Ann tag v

(<@) :: p -> f (Attr f p) -> Attr f p
(<@) = fixAnn

toMuExprF :: Expr a -> MuExprF
toMuExprF (EVar v) = Fix $ FVar v
toMuExprF (EInt v) = Fix $ FInt v
toMuExprF (EDouble v) = Fix $ FDouble v
toMuExprF (EAdd x y) = Fix $ FAdd (toMuExprF x) (toMuExprF y)
toMuExprF (ESub x y) = Fix $ FSub (toMuExprF x) (toMuExprF y)
toMuExprF (EMul x y) = Fix $ FMul (toMuExprF x) (toMuExprF y)
toMuExprF (EMin x y) = Fix $ FMin (toMuExprF x) (toMuExprF y)
toMuExprF (EMax x y) = Fix $ FMax (toMuExprF x) (toMuExprF y)
toMuExprF (EAt at) = atToFAt at
  where
    atToFAt :: forall d a. At d a -> MuExprF
    atToFAt (At {..}) =
      Fix $ FAt (spaceID space) $ toMuExprF <$> coordinatesToList @d coordinates
toMuExprF (ECast cast x) = Fix $ FCast (castToCastSimple cast) (toMuExprF x)

toAST :: Expr a -> Attr ExprF Tag
toAST = synthetise f . toMuExprF
  where
    f (FVar v) = mempty {vars = [v]}
    f (FAt space _) = mempty {spaces = [space]}
    f _ = mempty

-- | An F-Algebra like explained
-- <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras here>.
type Algebra f a = (Functor f) => (f a -> a)

getTag :: AST -> Tag
getTag = cata go
  where
    go (Ann tag (FAdd f g)) = tag <> f <> g
    go (Ann tag (FSub f g)) = tag <> f <> g
    go (Ann tag (FMul f g)) = tag <> f <> g
    go (Ann tag (FMin f g)) = tag <> f <> g
    go (Ann tag (FMax f g)) = tag <> f <> g
    go (Ann tag (FAt _ g)) = tag <> mconcat g
    go (Ann tag (FCast _ g)) = tag <> g
    go x = attr x

compileAST :: AST -> Text
compileAST = cata (go . unAnn)
  where
    go (FVar v) = unVar v
    go (FInt v) = pack $ show v
    go (FDouble v) = pack $ show v
    go (FAdd f g) = wrapInParens f <> " + " <> wrapInParens g
    go (FSub f g) = wrapInParens f <> " - " <> wrapInParens g
    go (FMul f g) = wrapInParens f <> " * " <> wrapInParens g
    go (FMin f g) = mconcat ["min", wrapInParens $ f <> ", " <> g]
    go (FMax f g) = mconcat ["max", wrapInParens $ f <> ", " <> g]
    go (FAt spaceName g) = spaceName <> wrapInParens (intercalate ", " g)
    go (FCast c g) = "<" <> c <> ">" <> wrapInParens g

compile :: Expr a -> Text
compile = toAST >>> compileAST

--------------------------------------------------------------------------------

testExpr :: Expr 'U32
testExpr =
  let x = Var "x"
      y = Var "y"
   in ECast u32 (EInt 2) * EAt (At gradient (N2 (EVar x) (EVar y)))

data SpaceType = Function | Image deriving (Eq)

isFunc :: Space d f => f d a -> Bool
isFunc s = spaceType s == Function

class (HasCoordinates d) => Space d f where
  spaceID :: f d a -> Text
  spaceType :: f d a -> SpaceType

instance Space 'D1 Func where
  spaceID = unFuncName . name
  spaceType _ = Function

instance Space 'D2 Func where
  spaceID = unFuncName . name
  spaceType _ = Function

instance Space 'D3 Func where
  spaceID = unFuncName . name
  spaceType _ = Function

instance Space 'D4 Func where
  spaceID = unFuncName . name
  spaceType _ = Function

--------------------------------------------------------------------------------

data Store = Store
  { counter :: Int,
    funcs :: Func (forall d. d :: Dimensions) (forall a. a)
  }

type AppM a = State Store a

getNextID :: Text -> AppM Text
getNextID prefix = do
  i <- gets counter
  modify $ counterL %~ (+) 1
  pure $ prefix <> "_" <> pack (show i)

counterL :: Lens' Store Int
counterL = lens counter (\s c -> s {counter = c})

newVar :: AppM Var
newVar = Var <$> getNextID "var_"

newFunc :: (InferArgs d -> Expr a) -> AppM (Func d a)
newFunc body = do
  name <- FuncName <$> getNextID "func_"
  pure $ Func {name, body}

--------------------------------------------------------------------------------

constant :: Func d 'U64
constant = Func {name = "blur_x", body = \_ -> EInt 9}

-- brighten :: Space 'D2 f => Double -> f 'D2 a -> Func 'D2 ('F64 :: ScalarType)
brighten :: (Space 'D2 f) => Double -> f 'D2 (a :: ScalarType) -> Func 'D2 'F64
brighten q space = Func {name = "brighten", body}
  where
    body (N2 x y) =
      EMul (EDouble q) $ EAt $ At space $ N2 (ECast u32 $ EVar x) (ECast u32 $ EVar y)

gradient :: Func 'D2 'U32
gradient = Func {name = "gradient", body}
  where
    body (N2 x y) = (EAdd `on` EVar) x y

wrapInParens :: Text -> Text
wrapInParens x = "(" <> x <> ")"

class Halogenide m a where
  render :: a -> m Text

class ExprType x where
  showType :: x -> Text

instance ExprType (Proxy 'U8) where
  showType _ = "UInt(8)"

instance ExprType (Proxy 'U16) where
  showType _ = "UInt(16)"

instance ExprType (Proxy 'U32) where
  showType _ = "UInt(32)"

instance ExprType (Proxy 'U64) where
  showType _ = "UInt(64)"

instance ExprType (Proxy 'F32) where
  showType _ = "Float(32)"

instance ExprType (Proxy 'F64) where
  showType _ = "Float(64)"

-- -- instance Halogenide Identity (Space (d :: Dimensions) f) where
-- --   render = f d a -> Text
--
-- instance Halogenide Identity (Expr (a :: ScalarType)) where
--   render (EVar (Var v)) = pure v
--   render (EInt i) = pure $ pack $ show i
--   render (EDouble d) = pure $ pack $ show d
--   render (EAdd e1 e2) = pure $ wrapInParens $ runIdentity $ render e1 <> " + " <> render e2
--   render (ESub e1 e2) = pure $ wrapInParens $ runIdentity $ render e1 <> " - " <> render e2
--   render (EMul e1 e2) = pure $ wrapInParens $ runIdentity $ render e1 <> " * " <> render e2
--   render (EMin e1 e2) = pure $ "min(" <> runIdentity (render e1) <> ", " <> runIdentity (render e2) <> ")"
--   render (EMax e1 e2) = "max(" <> render e1 <> ", " <> render e2 <> ")"
--   render (EAt (At f pos)) = pure $ spaceID f <> "(" <> intercalate ", " render (coordinatesToList pos)
--   render (ECast (_, tType) e) = pure $ "cast(" <> tType <> "," <> runIdentity (render e) <> ")"

gradientFunc :: Func 'D2 'U32
gradientFunc = Func {name = "gradient", body}
  where
    body (N2 x y) = ECast u32 $ (EAdd `on` EVar) x y

-- chatpgt, this is the piece that needs some more code in order to compile.
-- testProgram :: Program
-- testProgram = do
--   gradient <- newFunc $ \(x, y) -> ECast u8 $ (EAdd `on` EVar) x y
--   output <- newOutput "filename.jpeg"
--   realize gradient output

--
-- {-
-- Func blur_3x3(Func input) {
--   Func blur_x, blur_y;
--   Var x, y, xi, yi;
--
--   // The algorithm - no storage or order
--   blur_x(x, y) = (input(x-1, y) + input(x, y) + input(x+1, y))/3;
--   blur_y(x, y) = (blur_x(x, y-1) + blur_x(x, y) + blur_x(x, y+1))/3;
--
--   // The schedule - defines order, locality; implies storage
--   blur_y.tile(x, y, xi, yi, 256, 32)
--         .vectorize(xi, 8).parallel(y);
--   blur_x.compute_at(blur_y, x).vectorize(x, 8);
--
--   return blur_y;
-- }
--
-- let exprx =
-- blurx <- newFunc exprx
-- blury <- newFunc expry
--

-- do

--
-- -}
--
--
