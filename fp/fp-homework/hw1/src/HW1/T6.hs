module HW1.T6
(
mcat,
epart
) where

import Data.Foldable (fold, foldl')

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl' fun (mempty, mempty)
    where
        fun :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
        fun (x,y) (Left a)  = (x<>a,y)
        fun (x,y) (Right b) = (x,y<>b)