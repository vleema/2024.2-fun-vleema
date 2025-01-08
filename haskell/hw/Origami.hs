{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Origami where

import Prelude hiding (
  all,
  and,
  any,
  concat,
  dropWhile,
  filter,
  foldl,
  foldl1,
  foldr,
  foldr1,
  length,
  map,
  maximum,
  minimum,
  or,
  product,
  reverse,
  scanl,
  scanr,
  sum,
  takeWhile,
 )

import Prelude qualified as P hiding (minimum, scanl, scanr)

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e (x : xs) = x `f` foldr f e xs
foldr f e _ = e

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e (x : xs) = foldl f (e `f` x) xs
foldl f e _ = e

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x : xs) = x `f` foldr1 f xs
foldr1 _ _ = error "List should have a least one element for foldr1"

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ _ = error "List should have a least one element for foldl1"

--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e (x : xs) = e : scanl f (e `f` x) xs
scanl f e _ = [e]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e (x : xs) =
  let rest = scanr f e xs
   in case rest of
        [e] -> x `f` e : rest
        (x' : _) -> x `f` x' : rest
scanr f e _ = [e]

--
-- Define all of the following functions as folds:
--

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldl (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x acc -> p x || acc) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x acc -> p x && acc) True

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldl (||) False

minimum :: (Ord a) => [a] -> a
minimum [] = error "There's no minimum in empty list"
minimum (x : xs) = foldr min x xs

-- minimum (x : xs) =  min x (minimum xs)

maximum :: (Ord a) => [a] -> a
maximum [] = error "There's no minimum in empty list"
maximum (x : xs) = foldr max x xs

length :: (Integral i) => [a] -> i
length = foldr (\_ acc -> 1 + acc) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else []) []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = foldr (\x acc -> if p x then acc else x : acc) []

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: (Integral i) => [i] -> (i, Maybe i)
semo = foldr f (0, Nothing)
 where
  f x (sum, mo)
    | even x = (x + sum, mo)
    | otherwise = (sum, max x <$> mo)

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: (Eq a) => [a] -> [a]
remdups = foldr nodupcons []
 where
  nodupcons w [] = [w]
  nodupcons w xs@(h : _)
    | w == h = xs
    | otherwise = w : xs

safeLast :: [a] -> Maybe a
safeLast = foldr f Nothing
 where
  f x (Just y) = Just y
  f x Nothing = Just x

-- dec2int [1,9,9,2] = 1992
dec2int :: (Integral i) => [i] -> i
dec2int = foldl concatNum 0

concatNum :: (Integral i) => i -> i -> i
concatNum x y = x * (10 ^ digits y) + y

digits :: (Integral i) => i -> i
digits n
  | n < 10 = 1
  | otherwise = 1 + digits (n `div` 10)

-- f : i -> i -> i
