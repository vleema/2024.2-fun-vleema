{-# LANGUAGE KindSignatures #-}

module FA where

import Prelude hiding (
  Applicative (..),
  Functor (..),
  fmap,
  liftA,
  liftA2,
  liftA3,
  pure,
  unzip,
  ($>),
  (*>),
  (<$),
  (<$>),
  (<&>),
  (<*),
  (<**>),
  (<*>),
 )

import Control.Applicative qualified as A
import Data.Functor qualified as F

----------------------------------------------------------------
-- Functor
----------------------------------------------------------------

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> (f a -> f b)

  (<$) :: b -> f a -> f b
  (<$) = fmap . const

{- LAWS

   fmap id = id
   fmap (f . g) = fmap f . fmap g

 -}

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip at = (fmap fst at, fmap snd at)

void :: (Functor f) => f a -> f ()
void = fmap $ const ()

-- syntactic associativity and precedence
infixl 4 <$>, $>, <$
infixl 1 <&>

----  Instances  -----------------------------------------------

-- List
instance Functor [] where
  fmap = map

-- Maybe
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

-- (α ×)
instance Functor ((,) a) where
  fmap :: (c -> b) -> (a, c) -> (a, b)
  fmap f (a, c) = (a, f c)

-- (α +)
instance Functor (Either a) where
  fmap :: (b -> c) -> Either a b -> Either a c
  fmap f (Right b) = Right $ f b
  fmap _ (Left a) = Left a

-- (r →)
instance Functor ((->) r) where
  fmap = (.)

-- IO
instance Functor IO where
  fmap f ax = do
    x <- ax
    pure $ f x

----------------------------------------------------------------
-- Applicative
----------------------------------------------------------------

class (Functor f) => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{- LAWS

   pure id <*> v = ??
   pure f <*> pure x = ??
   u <*> pure y = ??
   u <*> (v <*> w) = ??

 -}

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA = fmap

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 = undefined

-- sequence actions, discarding the value of the first argument
(*>) :: (Applicative f) => f a -> f b -> f b
(*>) = undefined

-- sequence actions, discarding the value of the second argument
(<*) :: (Applicative f) => f a -> f b -> f a
(<*) = undefined

-- A variant of (<*>) with the types of the arguments reversed.
-- It differs from flip (<*>) in that the effects are resolved
-- in the order the arguments are presented.
(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = undefined

when :: (Applicative f) => Bool -> f () -> f ()
when = undefined

unless :: (Applicative f) => Bool -> f () -> f ()
unless = undefined

sequenceAL :: (Applicative f) => [f a] -> f [a]
sequenceAL = undefined

-- syntactic associativity and precedence
infixl 4 <*>, *>, <*, <**>

----  Instances  -----------------------------------------------

-- Maybe
instance Applicative Maybe where
  pure = Just
  Just f <*> Just a = Just $ f a
  _ <*> _ = Nothing

-- Lists with ambiguous computational aspect (non-determinism):
-- Create an isomorphic copy of the type List a, called Ambiguous a
newtype Ambiguous a = Ambiguous [a]

-- show ambiguous lists like this: ?[1,2,3]
instance (Show a) => Show (Ambiguous a) where
  show (Ambiguous xs) = "?" <> show xs

instance Functor Ambiguous where
  fmap = undefined

instance Applicative Ambiguous where
  pure = undefined
  (<*>) = undefined

-- Lists with temporal computational aspect (sequencial):
-- Create an isomorphic copy of the type List a, called Temporal a
-- (the isomorphism is given by the constructor/wrapper Temporal : List a -> Temporal a)
newtype Temporal a = Temporal [a]

-- show temporal lists like this: →[1,2,3]
instance (Show a) => Show (Temporal a) where
  show (Temporal xs) = "→" <> show xs

instance Functor Temporal where
  fmap = undefined

instance Applicative Temporal where
  pure = undefined
  (<*>) = undefined

-- IO
instance Applicative IO where
  pure = undefined
  (<*>) = undefined

-- (m ×)
instance (Monoid m) => Applicative ((,) m) where
  pure = undefined
  (<*>) = undefined

-- (s +)
instance (Semigroup s) => Applicative (Either s) where
  pure = undefined
  (<*>) = undefined

-- (r →)
instance Applicative ((->) r) where
  pure = undefined
  (<*>) = undefined
