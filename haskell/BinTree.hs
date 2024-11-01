module BinTree where

import Nat

data BinTree a where
  Node :: a -> Maybe (BinTree a) -> Maybe (BinTree a) -> BinTree a

preOrder :: (Show a) => BinTree a -> String
preOrder (Node value leftTree rightTree) =
  show value ++ maybe "" preOrder leftTree ++ maybe "" preOrder rightTree

symOrder :: (Show a) => BinTree a -> String
symOrder (Node value leftTree rightTree) =
  maybe "" preOrder leftTree ++ show value ++ maybe "" preOrder rightTree

postOrder :: (Show a) => BinTree a -> String
postOrder (Node value leftTree rightTree) =
  maybe "" preOrder leftTree ++ maybe "" preOrder rightTree ++ show value

levelOrder :: (Show a) => BinTree a -> String
levelOrder binTree = concat [showCurrentLevel level binTree | level <- [0 .. height binTree]]

showCurrentLevel :: (Show a) => Int -> BinTree a -> String
showCurrentLevel 0 (Node value _ _) = show value
showCurrentLevel n (Node _ leftTree rightTree) = maybe "" (showCurrentLevel (n - 1)) leftTree ++ maybe "" (showCurrentLevel (n - 1)) rightTree

height :: BinTree a -> Int
height (Node _ leftTree rightTree) = 1 + max (maybe 0 height leftTree) (maybe 0 height rightTree)

exampleTree1 :: BinTree Int
exampleTree1 =
  Node
    1
    (Just (Node 2 (Just (Node 4 Nothing Nothing)) Nothing))
    (Just (Node 3 (Just (Node 5 Nothing Nothing)) (Just (Node 6 Nothing Nothing))))

{-
       1
      / \
     2   3
    /   / \
   4   5   6
 - -}

exampleTree2 :: BinTree Char
exampleTree2 =
  Node
    'A'
    ( Just
        ( Node
            'B'
            (Just (Node 'D' Nothing (Just (Node 'J' Nothing Nothing))))
            (Just (Node 'H' Nothing Nothing))
        )
    )
    ( Just
        ( Node
            'C'
            (Just (Node 'E' Nothing Nothing))
            (Just (Node 'F' Nothing (Just (Node 'I' Nothing Nothing))))
        )
    )

{-
        A
       / \
      B   C
     / \   / \
    D   H E   F
     \         \
      J         I
 - -}

-- uma fila sao duas pilhas
