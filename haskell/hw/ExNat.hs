{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude (
  Bool (..),
  Eq (..),
  Integral (..),
  Num (..),
  Ord (..),
  Show (..),
  error,
  not,
  otherwise,
  undefined,
  ($),
  (&&),
  (++),
  (.),
  (||),
 )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show O = "O"
  show (S n) = "S" ++ show n

instance Eq Nat where
  O == O = True
  (S n) == (S m) = n == m
  _ == _ = False

instance Ord Nat where
  O <= _ = True
  _ <= O = False
  (S n) <= (S m) = n <= m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min O _ = O
  min _ O = O
  min (S n) (S m) = min n m

  max O n = n
  max n O = n
  max (S n) (S m) = max n m

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O <+> n = n
(S n) <+> m = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
O <-> m = O
n <-> O = n
(S n) <-> (S m) = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
O <*> _ = O
(S n) <*> m = m <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
n <^> (S m) = (n <^> m) <*> n

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "Division by 0 is undefined"
n </> m = if m >= n then O else S $ (n <-> m) </> m

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "Division by 0 is undefined"
n <%> m = if m > n then n else (n <-> m) <%> m

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = n <%> m == O

divides = (<|>)

gcd :: Nat -> Nat -> Nat
gcd n O = n
gcd a b = gcd b (a <%> b)

lcm :: Nat -> Nat -> Nat
lcm a b = (a * b) </> gcd a b

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff a b = if a < b then b <-> a else a <-> b

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n * factorial n

fibonacci :: Nat -> Nat
fibonacci O = O
fibonacci (S O) = S O
fibonacci (S (S n)) = fibonacci (S n) <+> fibonacci n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = error "There is no Logarithm of 0"
lo _ (S O) = O
lo b a = if b < a then O else S $ lo b (a </> b)

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat i = if i > 0 then S $ toNat (i - 1) else error "There is no negative natural number"

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = error "There's no negative natural number"
    | x == 0 = O
    | otherwise = S $ fromInteger (x - 1)
