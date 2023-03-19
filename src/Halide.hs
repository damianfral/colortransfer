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
{-# LANGUAGE TypeOperators #-}

module Halide where

import Control.Arrow ((>>>))
import Control.Lens (Lens', lens, (%~))
import Control.Monad.State (State, gets, modify)
import Data.Data (Proxy (..))
import Data.Foldable (Foldable (toList), fold)
import Data.Function (on)
import Data.Generics.Fixplate
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.String (IsString)
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | names to use as variables in the definition of a Func.
newtype Var = Var {unVar :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

newtype FuncName = FuncName {unFuncName :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

data Buffer = Buffer {bufferName :: Text, bufferFunc :: Func', bufferSize :: [Int]}
  deriving stock (Generic)

newtype Param (a :: ScalarType) = Param Text
  deriving (Show)

newtype ImageParam (d :: Dimensions) (a :: ScalarType) = ImageParam Text
  deriving (Show)

-- | A Func represents a pipeline stage.
data Func (d :: Dimensions) (a :: ScalarType) = Func
  { funcName :: FuncName,
    funcBody :: InferArgs d -> Expr a,
    funcArgs :: InferArgs d
  }

--- | A wrapper over `Func d a` to hide the type variables.
data Func' = forall d a. Func' (Func d a)

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

-- | @Expr's are complex expressions using @Vars.
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

newtype N1 a = N1 a deriving (Functor)

data N2 a = N2 a a deriving (Functor)

data N3 a = N3 a a a deriving (Functor)

data N4 a = N4 a a a a deriving (Functor)

class HasCoordinates d where
  type Coordinates d
  coordinatesToList :: Coordinates d -> [Expr 'U32]

instance HasCoordinates 'D0 where
  type Coordinates 'D0 = N0 (Expr 'U32)
  coordinatesToList _ = []

instance HasCoordinates 'D1 where
  type Coordinates 'D1 = N1 (Expr 'U32)
  coordinatesToList = toList

instance HasCoordinates 'D2 where
  type Coordinates 'D2 = N2 (Expr 'U32)
  coordinatesToList = toList

instance HasCoordinates 'D3 where
  type Coordinates 'D3 = N3 (Expr 'U32)
  coordinatesToList = toList

instance HasCoordinates 'D4 where
  type Coordinates 'D4 = N4 (Expr 'U32)
  coordinatesToList = toList

instance Foldable N0 where
  foldMap _ N0 = mempty

instance Foldable N1 where
  foldMap f (N1 v) = f v

instance Foldable N2 where
  foldMap f (N2 v1 v2) = foldMap f [v1, v2]

instance Foldable N3 where
  foldMap f (N3 v1 v2 v3) = foldMap f [v1, v2, v3]

instance Foldable N4 where
  foldMap f (N4 v1 v2 v3 v4) = foldMap f [v1, v2, v3, v4]

--------------------------------------------------------------------------------

data At d a = forall f.
  Space d f =>
  At
  { space :: f d a,
    coordinates :: Coordinates d,
    func :: Maybe (Func d a)
  }

data At' = forall d a. At' {at' :: At d a}

data ExprF f
  = FVar Var
  | FInt Int
  | FDouble Double
  | FAdd f f
  | FSub f f
  | FMul f f
  | FMin f f
  | FMax f f
  | FAt At' [f]
  | FCast CastSimple f
  deriving (Functor)

data Tag = Tag
  { vars :: [Var],
    params :: [Text],
    spaces :: [Text],
    funcs :: [Func']
  }

instance Semigroup Tag where
  t1 <> t2 =
    Tag
      { vars = ((<>) `on` vars) t1 t2,
        params = ((<>) `on` params) t1 t2,
        spaces = ((<>) `on` spaces) t1 t2,
        funcs = ((<>) `on` funcs) t1 t2
      }

instance Monoid Tag where mempty = Tag [] [] [] []

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
toMuExprF (EAt a) = atToFAt a
  where
    atToFAt :: forall d a. At d a -> MuExprF
    atToFAt b@(At {..}) =
      Fix $ FAt (At' b) $ toMuExprF <$> coordinatesToList @d coordinates
toMuExprF (ECast cast x) = Fix $ FCast (castToCastSimple cast) (toMuExprF x)

toAST :: Expr a -> Attr ExprF Tag
toAST = synthetise f . toMuExprF
  where
    f (FVar v) = mempty {vars = [v]}
    f (FAt (At' a) _) =
      case func a of
        Just g -> mempty {funcs = [Func' g], vars = funcVars g}
        Nothing -> mempty -- {spaces = spaceID $ space at}
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
    go (Ann tag (FAt (At' _) g)) = tag <> mconcat g
    go (Ann tag (FCast _ g)) = tag <> g
    go x = attr x

funcVars :: InferArgs d ~ args Var => Func d a -> [Var]
funcVars func = toList $ funcArgs func

compileAST :: AST -> Text
compileAST ast =
  T.unlines
    [ compileDeclarations $ getDeclarations ast,
      cata (go . unAnn) ast
    ]
  where
    go (FVar v) = unVar v
    go (FInt v) = pack $ show v
    go (FDouble v) = pack $ show v
    go (FAdd f g) = wrapInParens f <> " + " <> wrapInParens g
    go (FSub f g) = wrapInParens f <> " - " <> wrapInParens g
    go (FMul f g) = wrapInParens f <> " * " <> wrapInParens g
    go (FMin f g) = mconcat ["min", wrapInParens $ f <> ", " <> g]
    go (FMax f g) = mconcat ["max", wrapInParens $ f <> ", " <> g]
    go (FAt (At' (At {space})) g) = spaceID space <> wrapInParens (intercalate ", " g)
    go (FCast c g) = "<" <> c <> ">" <> wrapInParens g

compileDeclarations :: Tag -> Text
compileDeclarations tag = T.unlines [varDeclarations, funcDeclarations]
  where
    varDeclarations = T.unlines $ declareVar <$> vars tag
    funcDeclarations = T.unlines $ declareFunc <$> funcs tag
    declareVar (Var x) = "Var " <> x <> ";"
    declareFunc (Func' f) = compileFunc f

compileFunc ::
  forall coordinates d a.
  coordinates Var ~ InferArgs d =>
  coordinates Var ->
  Func d a ->
  Text
compileFunc coordinates Func {..} =
  T.unlines
    [ "Func " <> unFuncName funcName <> ";",
      unFuncName funcName
        <> "( "
        <> intercalate ", " (unVar <$> toList @coordinates coordinates)
        <> " ) "
        <> " = "
        <> rSide
        <> ";"
    ]
  where
    rSide :: Text
    rSide = compileAST $ toAST $ funcBody coordinates

getDeclarations :: AST -> Tag
getDeclarations = cata go
  where
    go :: Ann ExprF Tag Tag -> Tag
    go (Ann tag (FAdd f g)) = tag <> f <> g
    go (Ann tag (FSub f g)) = tag <> f <> g
    go (Ann tag (FMul f g)) = tag <> f <> g
    go (Ann tag (FMax f g)) = tag <> f <> g
    go (Ann tag (FMin f g)) = tag <> f <> g
    go (Ann tag (FAt _ g)) = tag <> fold g
    go (Ann tag (FCast _ g)) = tag <> g
    go (Ann tag _) = tag

compile :: Expr a -> Text
compile = toAST >>> compileAST

--------------------------------------------------------------------------------

testExpr :: Expr 'U32
testExpr =
  let x = Var "x"
      y = Var "y"
      args = N2 x y
   in ECast u32 (EInt 2) * EAt (at (gradient args) (EVar <$> args))

class (HasCoordinates d) => Space d f where
  spaceID :: f d a -> Text
  at :: f d a -> Coordinates d -> At d a

instance Space 'D1 Func where
  spaceID = unFuncName . funcName
  at f c = At {space = f, coordinates = c, func = Just f}

instance Space 'D2 Func where
  spaceID = unFuncName . funcName
  at f c = At {space = f, coordinates = c, func = Just f}

instance Space 'D3 Func where
  spaceID = unFuncName . funcName
  at f c = At {space = f, coordinates = c, func = Just f}

instance Space 'D4 Func where
  spaceID = unFuncName . funcName
  at f c = At {space = f, coordinates = c, func = Just f}

--------------------------------------------------------------------------------

newtype Store = Store {counter :: Int} deriving (Generic)

type AppM a = State Store a

getNextID :: Text -> AppM Text
getNextID prefix = do
  modify $ #counter %~ (+) 1
  i <- gets counter
  pure $ prefix <> "_" <> pack (show i)

counterL :: Lens' Store Int
counterL = lens counter (\s c -> s {counter = c})

newVar :: AppM Var
newVar = Var <$> getNextID "var_"

newFunc :: forall d a. InferArgs d -> (InferArgs d -> Expr a) -> AppM (Func d a)
newFunc funcArgs funcBody = do
  funcName <- FuncName <$> getNextID "func_"
  let func :: Func d a = Func {funcName, funcBody, funcArgs}
  pure func

realize :: Coordinates d -> Func d a -> Buffer
realize coords func = Buffer {bufferName, bufferFunc, bufferSize}
  where
    bufferName = undefined
    bufferFunc = undefined
    bufferSize = undefined

--------------------------------------------------------------------------------

constant :: InferArgs d -> Func d 'U64
constant funcArgs = Func {funcName = "blur_x", funcBody = \_ -> EInt 9, funcArgs}

brighten :: (Space 'D2 f) => Double -> N2 Var -> f 'D2 (a :: ScalarType) -> Func 'D2 'F64
brighten q funcArgs space = Func {funcName = "brighten", funcBody, funcArgs}
  where
    funcBody (N2 x y) =
      EMul (EDouble q) $ EAt $ At space (N2 (ECast u32 $ EVar x) (ECast u32 $ EVar y)) Nothing

gradient :: N2 Var -> Func 'D2 'U32
gradient funcArgs = Func {funcName = "gradient", funcBody, funcArgs}
  where
    funcBody (N2 x y) = (EAdd `on` EVar) x y

wrapInParens :: Text -> Text
wrapInParens x = "(" <> x <> ")"

gradientFunc :: N2 Var -> Func 'D2 'U32
gradientFunc funcArgs = Func {funcName = "gradient", funcBody, funcArgs}
  where
    funcBody (N2 x y) = ECast u32 $ (EAdd `on` EVar) x y

-- Escribe uan funcion para calcular rel factorial
