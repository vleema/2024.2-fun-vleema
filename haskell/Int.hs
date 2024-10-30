{-# LANGUAGE GADTs #-}

module Int where

import Prelude hiding (Int)

import Nat

data Int where
  Int :: (Nat, Nat) -> Int

instance Eq Int where
  Int (n, m) == Int (n', m') = n + m' == m + n'

instance Ord Int where
  Int (n, m) <= Int (n', m') = (n + m') <= (m + n')

instance Enum Int where
  toEnum x
    | x >= 0 = Int (toNat x, 0)
    | otherwise = Int (0, toNat (abs x))
  fromEnum (Int (n, m)) = fromNat n - fromNat m

instance Num Int where
  (+) = addInt
  (*) = multInt
  (-) = subInt
  abs (Int (n, m)) = if m > n then Int (m, n) else Int (n, m)
  fromInteger x
    | x >= 0 = Int (fromInteger x, 0)
    | otherwise = Int (0, fromInteger (abs x))

  signum (Int (n, m))
    | m > n = Int (0, 1)
    | m == n = Int (0, 0)
    | m <= n = Int (1, 0)

instance Show Int where
  show (Int (n, m)) = show (fromEnum (Int (n, m)))

addInt :: Int -> Int -> Int
addInt (Int (n, m)) (Int (n', m')) = Int (n + n', m + m')

subInt :: Int -> Int -> Int
subInt (Int (n, m)) (Int (n', m')) = Int (n + m', m + n')

multInt :: Int -> Int -> Int
multInt (Int (n, m)) (Int (n', m')) = Int (n * n' + m * m', n * m' + m * n')
