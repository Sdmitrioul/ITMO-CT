module HW1.T3
(
Tree(..),
mkBranch,
tsize,
tdepth,
tmember,
tinsert,
tFromList,
) where

import Prelude
import Numeric.Natural

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                      = 0
tdepth (Branch (_, depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ left head right) =
                                    if a == head
                                       then True
                                    else if a > head
                                            then tmember a right
                                         else tmember a left

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left a right = Branch ((tsize left) + (tsize right) + 1, (max (tdepth left) (tdepth right)) + 1) left a right

diffHeight :: Tree a -> Int
diffHeight Leaf                       = 0
diffHeight (Branch _ left _ right) = (tdepth right) - (tdepth left)

rotateRight :: Tree a -> Tree a
rotateRight Leaf                                      = Leaf
rotateRight (Branch _ Leaf _ _)                       = error "unreachable"
rotateRight (Branch _ (Branch _ ll lh lr) head right) = mkBranch ll lh (mkBranch lr head right)

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf                                     = Leaf
rotateLeft (Branch _ _ _ Leaf)                      = error "unreachable"
rotateLeft (Branch _ left head (Branch _ rl rh rr)) = mkBranch (mkBranch left head rl) rh rr

balanceTree :: Tree a -> Tree a
balanceTree Leaf                      = Leaf
balanceTree tree@(Branch _ left head right)
            | (treeDiff == 2) && (rightDiff < 0) = rotateLeft (mkBranch left head (rotateRight right))
            | (treeDiff == 2)                    = rotateLeft tree
            | (treeDiff == -2) && (leftDiff > 0) = rotateRight (mkBranch (rotateLeft left) head right)
            | (treeDiff == -2)                   = rotateRight tree
            | otherwise = tree
            where treeDiff  = diffHeight tree
                  rightDiff = diffHeight right
                  leftDiff  = diffHeight left

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = Branch (1, 1) Leaf a Leaf
tinsert a (Branch n left head right) = if a == head
                                          then Branch n left head right
                                       else if a > head
                                          then balanceTree (mkBranch left head (tinsert a right))
                                       else balanceTree (mkBranch (tinsert a left) head right)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
