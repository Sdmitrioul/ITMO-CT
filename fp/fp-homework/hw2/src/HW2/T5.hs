module HW2.T5
  (
    EvaluationError (..)
  , ExceptState (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , wrapExceptState
  ) where

import Control.Monad
import           HW2.T1              (Annotated (..), Except (..), mapExcept, mapAnnotated)
import           HW2.T4              (Expr (..),
                                      Prim (..))

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES (\x -> mapExcept (mapAnnotated f) (g x))

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (\s -> Success (x :# s))

joinHelper :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
joinHelper (Error err)          = Error err
joinHelper (Success (inner:#s)) = runES inner s

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState outer = ES (\s -> joinHelper (runES outer s))

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# (f s)))

throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES (\x -> Error err)

instance Functor (ExceptState e s) where
   fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure    = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero deriving (Show)

checkOnDiv :: Prim a -> Bool
checkOnDiv (Div _ _) = True
checkOnDiv _         = False

evalUn :: (Double -> Prim Double)
       -> (Double -> Double)
       -> Expr
       -> ExceptState EvaluationError [Prim Double] Double
evalUn c op exp = do
                    val <- eval exp
                    modifyExceptState (\s -> (c val):s)
                    return (op val)

evalBin :: (Double -> Double -> Prim Double)
        -> (Double -> Double -> Double)
        -> Expr
        -> Expr
        -> ExceptState EvaluationError [Prim Double] Double
evalBin c op left right = do
                            nLeft <- eval left
                            nRight <- eval right
                            modifyExceptState (\s -> (c nLeft nRight):s)
                            return (op nLeft nRight)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)        = pure x
eval (Op (Abs x))   = evalUn Abs abs x
eval (Op (Sgn x))   = evalUn Sgn signum x
eval (Op (Add x y)) = evalBin Add (+) x y
eval (Op (Sub x y)) = evalBin Sub (-) x y
eval (Op (Mul x y)) = evalBin Mul (*) x y
eval (Op (Div x y)) = do
                        nLeft <- eval x
                        nRight <- eval y
                        if nRight == 0
                          then throwExceptState DivideByZero
                        else do
                          modifyExceptState (\s -> (Div nLeft nRight):s)
                          return (nLeft / nRight)
