module Tree where

import BinTree qualified as B
import Direction

class Tree t where
  search :: (Ord a) => a -> t a -> Maybe Path
  insert :: (Ord a) => a -> t a -> t a
  remove :: (Ord a) => a -> t a -> t a
  rotateLeft :: t a -> t a
  rotateRight :: t a -> t a

instance Tree B.BinTree where
  search = B.search
  insert = B.insert
  remove = B.remove
  rotateLeft = B.rotateLeft
  rotateRight = B.rotateRight
