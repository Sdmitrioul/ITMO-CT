module HW1.T7
(
DotString(..),
Fun(..),
Inclusive(..),
ListPlus(..)
) where

data ListPlus a = a :+ ListPlus a | Last a deriving (Show)
infixr 5 :+

instance Semigroup (ListPlus a) where
    (Last x) <> y = x :+ y
    (h :+ t) <> y = h :+ (t <> y)

data Inclusive a b = This a | That b | Both a b deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
    (This x) <> (This y)     = This (x <> y)
    (This x) <> (That y)     = Both x y
    (This x) <> (Both y z)   = Both (x <> y) z
    (That x) <> (This y)     = Both y x
    (That x) <> (That y)     = That (x <> y)
    (That x) <> (Both y z)   = Both y (x <> z)
    (Both x y) <> (This z)   = Both (x <> z) y
    (Both x y) <> (That z)   = Both x (y <> z)
    (Both x y) <> (Both z t) = Both (x <> z) (y <> t)

newtype DotString = DS String deriving Show

instance Semigroup DotString where
    (DS "") <> (DS x) = DS x
    (DS x) <> (DS "") = DS x
    (DS x) <> (DS y) = DS (x ++ "." ++ y)

instance Monoid DotString where
    mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    (F x) <> (F y) = F (x . y)

instance Monoid (Fun a) where
    mempty = F id
