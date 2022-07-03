module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function (fix)
import           GHC.Natural

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' g = fix (\f arr -> case arr of
  []    -> []
  (h:t) -> (g h):(f t))

fib :: Natural -> Natural
fib = fix (\f cur next n -> if n < 1 then cur else f next (cur + next) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\f n -> if n <= 1 then 1 else n * (f (n - 1)))
