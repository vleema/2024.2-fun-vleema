{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

module ExList where

import Data.Char qualified as C
import Data.List qualified as L
import Data.Text.Lazy (singleton)
import Prelude (
  Bool (..),
  Char,
  Double,
  Enum (..),
  Eq (..),
  Float,
  Int,
  Integer,
  Integral (..),
  Num (..),
  Ord (..),
  String,
  curry,
  error,
  flip,
  not,
  otherwise,
  uncurry,
  undefined,
  ($),
  (&&),
  (.),
  (||),
 )
import Prelude qualified as P

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}

{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Tried using head on empty List"
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "Tried using tail on empty list"
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: (Integral i) => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ l = l
(x : xs) ++ l = x : (xs ++ l)

-- right-associative for performance!
-- (what?!) XD
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

minimum :: (Ord a) => [a] -> a
minimum [] = error "Tried to get minimum empty list"
minimum (x : xs)
  | x <= minimum xs = x
  | otherwise = minimum xs

maximum :: (Ord a) => [a] -> a
maximum [] = error "Tried to get maximum at empty list"
maximum (x : xs)
  | x >= maximum xs = x
  | otherwise = maximum xs

take :: (Integral i) => i -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs

drop :: (Integral i) => i -> [a] -> [a]
drop 0 lst = lst
drop n (x : xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile pred (x : xs)
  | pred x = x : takeWhile pred xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile pred (x : xs)
  | pred x = dropWhile pred xs
  | otherwise = x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = error "Tried to use init in empty list"
init lst = take (length lst - 1) lst

inits :: [a] -> [[a]]
inits = reverse . tails

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) =
  let tailSubseq = subsequences xs
   in map (x :) tailSubseq ++ tailSubseq

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any pred (x : xs) = pred x || any pred xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all pred (x : xs) = pred x && all pred xs

and :: [Bool] -> Bool
and [] = True
and (bool : bools) = bool && and bools

or :: [Bool] -> Bool
or [] = False
or (bool : bools) = bool || or bools

concat :: [[a]] -> [a]
concat [] = []
concat (list : lists) = list ++ concat lists

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any (x ==)

-- elem': same as elem but elementary definition
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) = a == x || elem' a xs

-- (without using other functions except (==))

-- (!!)
(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(x : xs) !! n
  | n >= length (x : xs) = error "Index out of bounds while using (!!)"
  | otherwise = xs !! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x : xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map func (x : xs) = func x : map func xs

cycle :: [a] -> [a]
cycle list = list ++ cycle list

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate n = take n . repeat

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (x : xs) (y : ys)
  | x == y = isInfixOf xs ys
  | otherwise = isInfixOf (x : xs) ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l1 l2 = reverse l1 `isPrefixOf` reverse l2

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith func (x : xs) (y : ys) = func x y : zipWith func xs ys

intercalate :: [a] -> [[a]] -> [a]
intercalate_ [x] = x
intercalate list (x : xs) = x ++ list ++ intercalate list xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

splitAt :: Int -> [a] -> ([a], [a])
splitAt i list = (take i list, drop i list)

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break pred list = (takeWhile pred list, dropWhile pred list)

lines :: String -> [String]
lines str =
  let (line, rest) = break (/= '\n') str
   in line : case rest of
        [] -> []
        (_ : xs) -> lines xs

words :: String -> [String]
words str =
  let (line, rest) = break isNotSpace str
   in case line of
        [] -> case rest of
          [] -> []
          (_ : xs) -> words xs
        _ -> line : words (dropWhile isNotSpace rest)
 where
  isNotSpace ch = not $ ch == '\n' || ch == '\t' || ch == ' '

unlines :: [String] -> String
unlines = intercalate "\n"

unwords :: [String] -> String
unwords = intercalate " "

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose xss = map head xss : transpose (map tail xss)

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome list = reverse list == list

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
