module HW1.T4
(
tfoldr,
treeToList,
) where

import Prelude
import HW1.T3


tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ res Leaf = res
tfoldr fun x (Branch _ left head right) = tfoldr fun (fun head (tfoldr fun x right)) left

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
