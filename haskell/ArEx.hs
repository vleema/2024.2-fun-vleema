{-# LANGUAGE GADTs #-}

module ArEx where

import Prelude (Int, negate, ($), (*), (+))

data ArEx where
  Int :: Int -> ArEx
  Plus :: Int -> ArEx -> ArEx
  Mult :: Int -> ArEx -> ArEx
  Minus :: ArEx -> ArEx

eval :: ArEx -> Int
eval (Int x) = x
eval (x `Plus` e) = x + eval e
eval (x `Mult` e) = x * eval e
eval (Minus e) = negate $ eval e

height :: ArEx -> Int
height (Int x) = 1
height (Minus x) = 1
height (x `Plus` e) = 1 + height e
height (x `Mult` e) = 1 + height e
