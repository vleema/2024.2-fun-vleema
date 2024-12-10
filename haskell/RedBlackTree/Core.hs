module RedBlackTree.Core where

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

data Direction where
  L :: Direction
  R :: Direction
  deriving (Show)

type Path = [Direction]

search :: (Ord a) => a -> RedBlackTree a -> Maybe Path
search _ Nil = Nothing
search a (Node value leftTree rightTree _)
  | a == value = Just []
  | a > value = fmap (R :) (search value rightTree)
  | a < value = fmap (L :) (search value leftTree)
