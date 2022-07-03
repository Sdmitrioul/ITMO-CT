module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import           GHC.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

--((a -> a) -> a -> a) -> (a -> a) -> a -> a
ns :: Nat a -> Nat a
ns b f x = f (b f x)

--((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
nplus :: Nat a -> Nat a -> Nat a
nplus a b f x = a f (b f x)

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a (b f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0