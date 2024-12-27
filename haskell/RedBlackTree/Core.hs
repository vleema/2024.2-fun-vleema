{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module RedBlackTree.Core where

import Data.Either (lefts)
import Direction (Direction (..), Path)
import GHC.TypeLits (Nat, type (+))

data Color = Red | Black

-- A RBTree encodes both the color and the "black-height" of the tree in its type.
data RedBlackTree (c :: Color) (h :: Nat) a where
  Nil :: RedBlackTree 'Black 0 a
  RedNode :: RedBlackTree 'Black h a -> a -> RedBlackTree 'Black h a -> RedBlackTree 'Red h a
  BlackNode :: RedBlackTree c h a -> a -> RedBlackTree c h a -> RedBlackTree 'Black (h + 1) a

search :: (Ord a) => a -> RedBlackTree c h a -> Maybe Path
search x (RedNode left root right) = search' x left root right
search x (BlackNode left root right) = search' x left root right

search' :: (Ord a) => a -> RedBlackTree c h a -> a -> RedBlackTree c h a -> Maybe Path
search' x left root right
  | x == root = Just []
  | x >= root = (R :) <$> search x right
  | x <= root = (L :) <$> search x left

--
-- data Color = Red | Black
--
-- data RedBlackTree a
--   = Node
--       { value :: a
--       , lefTree :: RedBlackTree a
--       , rightTree :: RedBlackTree a
--       , color :: Color
--       }
--   | Nil
--
-- search :: (Ord a) => a -> RedBlackTree a -> Maybe Path
-- search _ Nil = Nothing
-- search a (Node value leftTree rightTree _)
--   | a == value = Just []
--   | a > value = (R :) <$> search value rightTree
--   | a < value = (L :) <$> search value leftTree
--
-- insert :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
-- insert value Nil = Node value Nil Nil Red
-- insert value (Node root leftTree rightTree rootColor)
--   | value == root = Node root leftTree rightTree rootColor
--   | value >= root = recolor (Node root leftTree (insert value rightTree))
--   | value <= root = recolor (Node root leftTree (insert value rightTree))
--  where
--   recolor :: RedBlackTree a -> RedBlackTree a
--
-- rotateLeft :: RedBlackTree a -> RedBlackTree a
-- rotateLeft (Node a leftTree (Node b bLeftTree bRightTree bColor) color) =
--   Node b (Node a leftTree bLeftTree color) bRightTree bColor
--
-- rotateRight :: RedBlackTree a -> RedBlackTree a
-- rotateRight (Node a (Node b bLeftTree bRightTree bColor) rightTree color) =
--   Node b bLeftTree (Node a bRightTree rightTree color) bColor
--
