{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Halide where

import Control.Monad.Free
import Control.Monad.State (MonadState (..), State, modify)
import Data.Data (Proxy (..))
import Data.Function (on)
import Data.Text (Text, intercalate, pack)

--------------------------------------------------------------------------------

newtype Var = Var Text

data Func (d :: Dimensions) (a :: ScalarType) = Func
  { name :: Text,
    body :: InferArgs d -> Expr a
  }

newtype Param ( a :: ScalarType ) = Param Text
newtype ImageParam (d :: Dimensions) (a:: ScalarType) = ImageParam Text
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

type family InferArgs a where
  InferArgs 'D0 = ()
  InferArgs 'D1 = Var
  InferArgs 'D2 = (Var, Var)
  InferArgs 'D3 = (Var, Var, Var)
  InferArgs 'D4 = (Var, Var, Var, Var)

type family InferCoordinates d where
  InferCoordinates 'D0 = ()
  InferCoordinates 'D1 = Expr 'U32
  InferCoordinates 'D2 = (Expr 'U32, Expr 'U32)
  InferCoordinates 'D3 = (Expr 'U32, Expr 'U32, Expr 'U32)
  InferCoordinates 'D4 = (Expr 'U32, Expr 'U32, Expr 'U32, Expr 'U32)

data Expr (a :: ScalarType) where
  EVar :: Var -> Expr 'U32
  EInt :: Int -> Expr 'U64
  EDouble :: Double -> Expr 'F64
  EAdd :: Expr a -> Expr b -> Expr (InferType a b)
  ESub :: Expr a -> Expr b -> Expr (InferType a b)
  EMul :: Expr a -> Expr b -> Expr (InferType a b)
  EMin :: Expr a -> Expr b -> Expr (InferType a b)
  EMax :: Expr a -> Expr b -> Expr (InferType a b)
  EAt :: Space d f => f d a -> InferCoordinates d -> Expr a
  ECast :: Cast (b :: ScalarType) -> Expr a -> Expr b

testExpr :: Expr 'U64
testExpr = EMul (EInt 2) (EAdd (EVar $ Var "x") (EInt 4))

class Space (d :: Dimensions) f where
  spaceID :: f d a -> Text
  spacePosition :: f d a -> InferCoordinates d -> Text

instance Space 'D1 Func where
  spaceID = name
  spacePosition s expr = spaceID s <> wrapInParens (render expr)

instance Space 'D2 Func where
  spaceID = name
  spacePosition s (e1, e2) =
    spaceID s
      <> wrapInParens (intercalate ", " $ render <$> [e1, e2])

instance Space 'D3 Func where
  spaceID = name
  spacePosition s (e1, e2, e3) =
    spaceID s
      <> wrapInParens (intercalate ", " $ render <$> [e1, e2, e3])

--------------------------------------------------------------------------------

newtype Counter a = Counter {runCounter :: State Int a}
  deriving newtype (Functor, Applicative, Monad, MonadState Int)

getNextID :: Text -> Counter Text
getNextID prefix = do
  i <- get
  modify $ (+) 1
  pure $ prefix <> "_" <> pack (show i)

newVar :: Counter Var
newVar = Var <$> getNextID "var_"

-- newFunc :: (InferArgs d -> Expr a) -> Counter (Func d a)
-- newFunc body = do
--   name <- getNextID "func_"
--   pure $ Func {name, body}

--------------------------------------------------------------------------------

constant :: Func d 'U64
constant = Func {name = "blur_x", body = \_ -> EInt 9}

-- brighten :: Space 'D2 f => Double -> f 'D2 a -> Func 'D2 ('F64 :: ScalarType)
brighten :: (Space 'D2 f) => Double -> f 'D2 (a :: ScalarType) -> Func 'D2 'F64
brighten q space = Func {name = "brighten", body}
  where
    body (x, y) =
      EMul (EDouble q) $ EAt space (ECast u32 $ EVar x, ECast u32 $ EVar y)

gradient :: Func 'D2 'U32
gradient = Func {name = "gradient", body}
  where
    body (x, y) = ECast u32 $ (EAdd `on` EVar) x y

wrapInParens :: Text -> Text
wrapInParens x = "(" <> x <> ")"

class Halogenide a where
  render :: a -> Text

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

instance Halogenide (Expr (a :: ScalarType)) where
  render (EVar (Var v)) = v
  render (EInt i) = pack $ show i
  render (EDouble d) = pack $ show d
  render (EAdd e1 e2) = wrapInParens $ render e1 <> " + " <> render e2
  render (ESub e1 e2) = wrapInParens $ render e1 <> " - " <> render e2
  render (EMul e1 e2) = wrapInParens $ render e1 <> " * " <> render e2
  render (EMin e1 e2) = "min(" <> render e1 <> ", " <> render e2 <> ")"
  render (EMax e1 e2) = "max(" <> render e1 <> ", " <> render e2 <> ")"
  render (EAt f pos) = spacePosition f pos
  render (ECast (_, tType) e) = "cast(" <> tType <> "," <> render e <> ")"

gradientFunc :: Func 'D2 'U32
gradientFunc = Func {name = "gradient", body}
  where
    body (x, y) = ECast u32 $ (EAdd `on` EVar) x y

-- chatpgt, this is the piece that needs some more code in order to compile.
testProgram :: Program
testProgram = do
  gradient <- newFunc $ \(x, y) -> ECast u8 $ (EAdd `on` EVar) x y
  output <- newOutput "filename.jpeg"
  realize gradient output

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
