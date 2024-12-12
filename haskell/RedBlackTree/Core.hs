module RedBlackTree.Core where

import Direction (Direction (..), Path)

data Color = Red | Black

data RedBlackTree a
  = Node
      { value :: a
      , lefTree :: RedBlackTree a
      , rightTree :: RedBlackTree a
      , color :: Color
      }
  | Nil

search :: (Ord a) => a -> RedBlackTree a -> Maybe Path
search _ Nil = Nothing
search a (Node value leftTree rightTree _)
  | a == value = Just []
  | a > value = (R :) <$> search value rightTree
  | a < value = (L :) <$> search value leftTree

rotateLeft :: RedBlackTree a -> RedBlackTree a
rotateLeft (Node a leftTree (Node b bLeftTree bRightTree bColor) color) =
  Node b (Node a leftTree bLeftTree color) bRightTree bColor

rotateRight :: RedBlackTree a -> RedBlackTree a
rotateRight (Node a (Node b bLeftTree bRightTree bColor) rightTree color) =
  Node b bLeftTree (Node a bRightTree rightTree color) bColor
