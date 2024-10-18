module ExBool where

-- Do not alter this import!
import Prelude (
  Char,
  Enum (..),
  Eq (..),
  Int,
  Integral (..),
  Num (..),
  Ord (..),
  Show (..),
  error,
  otherwise,
  undefined,
  ($),
  (++),
  (.),
 )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True deriving (Eq)

instance Show Bool where
  show True = "True"
  show False = "False"

instance Enum Bool where
  toEnum 1 = True
  toEnum 0 = False

  fromEnum True = 1
  fromEnum False = 0

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) b = not . (&&) b

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) b = not . (||) b

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) b d
  | b == d = False
  | otherwise = True

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True f _ = f
ifThenElse False _ g = g

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
p <== q = q ==> p

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) p q = (p ==> q) && (p <== q)

infixr 1 <=>
