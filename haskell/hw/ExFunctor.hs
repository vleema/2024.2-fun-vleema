module ExFunctor where

import BinTree
import Prelude hiding (fmap, (<$))

data Pair a b where
  Pair :: a -> b -> Pair a b

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b -> f a -> f b
  (<$) = fmap . const

instance Funktor [] where
  fmap = map

instance Funktor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just $ f x

-- what about Either?
instance Funktor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap _ (Left x) = Left x

-- what about pairs?
instance Funktor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

-- what about functions?

-- what about Trees?
instance Funktor BinTree where
  fmap _ Nil = Nil
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- ...define Functor instances of as many * -> * things as you can think of!
