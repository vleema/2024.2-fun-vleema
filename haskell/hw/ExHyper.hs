module ExHyper where

import Prelude hiding (exp)

-- Nat datatype --------------------------------

data Nat = O | S Nat
  deriving (Eq)

instance (Num Nat) where
  (+) = add
  (*) = mul
  abs = id
  fromInteger 0 = O
  fromInteger n
    | n > 0 = S $ fromInteger (n - 1)
    | otherwise = O
  signum O = O
  signum n = S O
  negate n = O

instance (Ord Nat) where
  O <= m = True
  (S n) <= O = False
  (S n) <= (S m) = n <= m

instance (Show Nat) where
  show 0 = "0"
  show (S n) = "S" ++ show n

{- explicit definitions of add, mul, exp:

add n O     = n
add n (S m) = S (add m n)

mul n O     = O
mul n (S m) = add (mul n m) n

exp n O     = S O
exp n (S m) = mul (exp n m) n

-}

------------------------------------------------

-- substitute 'undefined' by the correct number
-- to define each of those functions:

add :: Nat -> Nat -> Nat
add = hyper 0

mul :: Nat -> Nat -> Nat
mul = hyper 1

exp :: Nat -> Nat -> Nat
exp = hyper 2

-- hyper n should return the n'th operation in the sequence:
-- (..?..), add, mul, exp, ...?

hyper :: (Integral i) => i -> (Nat -> Nat -> Nat)
hyper 0 =
  let add a b =
        case b of
          0 -> a
          (S predB) -> S (add a predB)
   in add
hyper 1 =
  let mul a b =
        case b of
          0 -> 0
          (S predB) -> hyper 0 (mul a predB) b
   in mul
hyper 2 =
  let exp base expoent =
        case expoent of
          0 -> 1
          (S predExpoent) -> hyper 1 base (exp base predExpoent)
   in exp
