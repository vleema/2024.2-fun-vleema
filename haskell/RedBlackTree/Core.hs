module RedBlackTree.Core where

import Direction (Direction (..), Path)

data Color where
  Red :: Color
  Black :: Color

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
  | a > value = fmap (R :) (search value rightTree)
  | a < value = fmap (L :) (search value leftTree)
