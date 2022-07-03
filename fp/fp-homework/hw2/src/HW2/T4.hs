module HW2.T4
  ( Expr (..)
  , Prim (..)
  , State (..)
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import HW2.T1 (Annotated (..), mapAnnotated, getExpr, getState)
import Control.Monad

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (\x -> mapAnnotated f (g x))

wrapState :: a -> State s a
wrapState x = S (\s -> (x:#s))

joinState :: State s (State s a) -> State s a
joinState outer = S (\s ->
                             let (inner:#s') = runS outer s
                             in  runS inner s')

modifyState :: (s -> s) -> State s ()
modifyState f = S (\x -> ():#(f x))

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- ^ (+)
  | Sub a a      -- ^ (-)
  | Mul a a      -- ^ (*)
  | Div a a      -- ^ (/)
  | Abs a        -- ^ abs
  | Sgn a        -- ^ signum
  deriving (Show)

data Expr =
  Val Double
  | Op (Prim Expr) deriving (Show)

instance Num Expr where
  x + y         = Op (Add x y)
  x * y         = Op (Mul x y)
  x - y         = Op (Sub x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  negate x      = Op (Sub (Val 0) x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y          = Op (Div x y)
  fromRational x = Val (fromRational x)

evalUn :: (Double -> Prim Double)
       -> (Double -> Double)
       -> Expr
       -> State [Prim Double] Double
evalUn c op exp = do
                  val <- eval exp
                  modifyState (\s -> (c val):s)
                  return (op val)

evalBin :: (Double -> Double -> Prim Double)
        -> (Double -> Double -> Double)
        -> Expr
        -> Expr
        -> State [Prim Double] Double
evalBin c op left right = do
                          nLeft <- eval left
                          nRight <- eval right
                          modifyState (\s -> (c nLeft nRight):s)
                          return (op nLeft nRight)

eval :: Expr -> State [Prim Double] Double
eval (Val x)        = pure x
eval (Op (Abs x))   = evalUn Abs abs x
eval (Op (Sgn x))   = evalUn Sgn signum x
eval (Op (Add x y)) = evalBin Add (+) x y
eval (Op (Sub x y)) = evalBin Sub (-) x y
eval (Op (Mul x y)) = evalBin Mul (*) x y
eval (Op (Div x y)) = evalBin Div (/) x y
