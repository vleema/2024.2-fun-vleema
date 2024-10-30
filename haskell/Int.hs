{-# LANGUAGE GADTs #-}

module Int where

import Prelude hiding (Int)

import Nat

data Int where
  MkInt :: (Nat, Nat) -> Int

instance Eq Int where
  MkInt (n, m) == MkInt (n', m') = n + m' == m + n'

instance Ord Int where
  MkInt (n, m) <= MkInt (n', m') = (n + m') <= (m + n')

instance Enum Int where
  toEnum x
    | x >= 0 = MkInt (toNat x, 0)
    | otherwise = MkInt (0, toNat (abs x))
  fromEnum (MkInt (n, m)) = fromNat n - fromNat m

instance Num Int where
  (+) = addInt
  (*) = multInt
  (-) = subInt
  abs (MkInt (n, m)) = if m > n then MkInt (m, n) else MkInt (n, m)
  fromInteger x
    | x >= 0 = MkInt (fromInteger x, 0)
    | otherwise = MkInt (0, fromInteger (abs x))

  signum (MkInt (n, m))
    | m > n = MkInt (0, 1)
    | m == n = MkInt (0, 0)
    | m <= n = MkInt (1, 0)

instance Show Int where
  show (MkInt (n, m)) = show (fromEnum (MkInt (n, m)))

addInt :: Int -> Int -> Int
addInt (MkInt (n, m)) (MkInt (n', m')) = MkInt (n + n', m + m')

subInt :: Int -> Int -> Int
subInt (MkInt (n, m)) (MkInt (n', m')) = MkInt (n + m', m + n')

multInt :: Int -> Int -> Int
multInt (MkInt (n, m)) (MkInt (n', m')) = MkInt (n * n' + m * m', n * m' + m * n')
