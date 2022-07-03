module HW1.T5
(
splitOn,
joinWith
) where

import Data.List.NonEmpty (NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sp = foldr chrSplit ([]:|[])
    where
        chrSplit ch (h:|t) = if ch == sp
                                then []:|(h:t)
                             else (ch:h):|t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sp (h:|[]) = h
joinWith sp (h:|t) = h++(joinBy sp t)

joinBy :: a -> [[a]] -> [a]
joinBy sp [] = []
joinBy sp (h:t) = [sp]++h++(joinBy sp t)