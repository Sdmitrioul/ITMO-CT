module HW1.T2
(
N(..),
nplus,
nmult,
nsub,
ncmp,
nFromNatural,
nToNum,
toNum,
nEven,
nOdd,
ndiv,
nmod
) where

import Prelude
import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N        -- addition
nplus l Z     = l
nplus Z l     = l
nplus l (S r) = S (nplus l r)

nmult :: N -> N -> N        -- multiplication
nmult Z _     = Z
nmult _ Z     = Z
nmult l (S Z) = l
nmult (S Z) l = l
nmult k (S r) = nplus k (nmult k r)

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub Z (S _)     = Nothing
nsub r Z         = Just r
nsub (S r) (S k) = nsub r k

ncmp :: N -> N -> Ordering
ncmp a b =
  case nsub a b of
    Nothing  -> LT
    (Just Z) -> EQ
    (Just _) -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + (nToNum n)

toNum :: Num a => Maybe N -> a
toNum Nothing = -1
toNum (Just r)  = nToNum r

nEven :: N -> Bool    -- parity checking
nEven Z         = True
nEven (S Z)     = False
nEven (S (S r)) = nEven r

nOdd :: N -> Bool     -- parity checking
nOdd x = not (nEven x)

ndiv :: N -> N -> N         -- integer division
ndiv x y = ndivH x y y Z

ndivH :: N -> N -> N -> N -> N
ndivH x y Z r = ndivH x y y (S r)
ndivH Z _ _ r = r
ndivH (S x) y (S z) r = ndivH x y z r


nmod :: N -> N -> N         -- modulo operation
nmod x y = nmodH x y y x

nmodH :: N -> N -> N -> N -> N
nmodH x y Z _ = nmodH x y y x
nmodH Z _ _ x = x
nmodH (S x) y (S z) r = nmodH x y z r