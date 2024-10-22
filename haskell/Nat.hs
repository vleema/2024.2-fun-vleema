module Nat where

import Prelude hiding (
  Num (..),
  div,
  gcd,
  max,
  min,
  quot,
  rem,
  (<),
 )

data Nat where
  O :: Nat
  S :: Nat -> Nat
  deriving (Eq)

-- abbrevs (syntactic sugar)
o, so, sso, ssso, sssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso
des = S (S (S (S (S (S (S (S (S (S O)))))))))

instance Show Nat where
  show O = "0"
  show (S O) = "1"
  show (S (S O)) = "2"
  show (S (S (S O))) = "3"
  show (S (S (S (S O)))) = "4"
  show (S (S (S (S (S O))))) = "5"
  show (S (S (S (S (S (S O)))))) = "6"
  show (S (S (S (S (S (S (S O))))))) = "7"
  show (S (S (S (S (S (S (S (S O)))))))) = "8"
  show (S (S (S (S (S (S (S (S (S O))))))))) = "9"
  show n = show (quot n des) ++ show (rem n des)

(+) :: Nat -> Nat -> Nat
O + n = n
(S n) + m = S (n + m)

(*) :: Nat -> Nat -> Nat
O * _ = o
(S n) * m = m + (n * m)

(-) :: Nat -> Nat -> Nat
O - m = O
n - O = n
(S n) - (S m) = n - m

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min O _ = O
min _ O = O
min (S n) (S m) = S (min n m)

(<) :: Nat -> Nat -> Bool
n < O = False
O < (S n) = True
(S n) < (S m)
  | n == m = False
  | otherwise = n < m

max :: Nat -> Nat -> Nat
max n O = n
max O m = m
max (S n) (S m) = S (max n m)

quot :: Nat -> Nat -> Nat
quot n O = error "Division by 0 is undefined"
quot O _ = O
quot n m
  | max n m == m = O
  | otherwise = S (quot (n - m) m)

rem :: Nat -> Nat -> Nat
rem m O = error "Division by 0 is undefined"
rem O _ = O
rem n m
  | n < m = n
  | otherwise = rem (n - m) m

div :: Nat -> Nat -> (Nat, Nat)
div n m = (quot n m, rem n m)

gcd :: Nat -> Nat -> Nat
gcd n O = n
gcd a b = gcd b (rem a b)

lcm :: Nat -> Nat -> Nat
lcm a b = (a * b) `quot` gcd a b
