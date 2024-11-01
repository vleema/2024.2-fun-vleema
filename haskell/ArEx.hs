{-# LANGUAGE GADTs #-}

module ArEx where

import Prelude (Int, max, negate, ($), (*), (+))

data ArEx where
  Int :: Int -> ArEx
  Plus :: ArEx -> ArEx -> ArEx
  Mult :: ArEx -> ArEx -> ArEx
  Minus :: ArEx -> ArEx

eval :: ArEx -> Int
eval (Int x) = x
eval (x `Plus` y) = eval x + eval y
eval (x `Mult` y) = eval x * eval y
eval (Minus e) = negate $ eval e

height :: ArEx -> Int
height (Int x) = 1
height (Minus x) = 1
height (x `Plus` y) = 1 + max (height x) (height y)
height (x `Mult` y) = 1 + max (height x) (height y)
