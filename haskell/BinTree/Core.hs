module BinTree.Core where

data BinTree a where
  Node :: a -> BinTree a -> BinTree a -> BinTree a
  Nil :: BinTree a

data Direction where
  L :: Direction
  R :: Direction
  deriving (Show)

type Path = [Direction]

preOrder :: (Show a) => BinTree a -> String
preOrder Nil = ""
preOrder (Node value leftTree rightTree) =
  show value ++ preOrder leftTree ++ preOrder rightTree

symOrder :: (Show a) => BinTree a -> String
symOrder Nil = ""
symOrder (Node value leftTree rightTree) =
  preOrder leftTree ++ show value ++ preOrder rightTree

postOrder :: (Show a) => BinTree a -> String
postOrder Nil = ""
postOrder (Node value leftTree rightTree) =
  preOrder leftTree ++ preOrder rightTree ++ show value

levelOrder :: (Show a) => BinTree a -> String
levelOrder binTree = concat [showLevel level binTree | level <- [0 .. height binTree]]

showLevel :: (Show a) => Int -> BinTree a -> String
showLevel _ Nil = ""
showLevel 0 (Node value _ _) = show value
showLevel n (Node _ leftTree rightTree) = showLevel (n - 1) leftTree ++ showLevel (n - 1) rightTree

height :: BinTree a -> Int
height Nil = 0
height (Node _ leftTree rightTree) = 1 + max (height leftTree) (height rightTree)

search :: (Ord a) => a -> BinTree a -> Maybe Path
search _ Nil = Nothing
search value (Node rootValue leftTree rightTree)
  | value == rootValue = Just []
  | value > rootValue = addPath R rightTree
  | value < rootValue = addPath L leftTree
 where
  addPath direction subtree =
    (direction :) <$> search value subtree

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert value Nil = Node value Nil Nil
insert value (Node rootValue leftTree rightTree)
  | value == rootValue = Node rootValue leftTree rightTree
  | value > rootValue = Node rootValue leftTree (insert value rightTree)
  | value < rootValue = Node rootValue (insert value leftTree) rightTree

remove :: (Ord a) => a -> BinTree a -> BinTree a
remove _ Nil = Nil
remove value (Node rootValue leftTree rightTree)
  | value > rootValue = Node rootValue leftTree $ remove value rightTree
  | value < rootValue = Node rootValue (remove value leftTree) rightTree
  | value == rootValue = case (leftTree, rightTree) of
      (leftTree, Nil) -> leftTree
      (Nil, rightTree) -> rightTree
      (leftTree, rightTree) -> Node leftMaximum alteredLeftTree rightTree
       where
        leftMaximum = case fetchRightMost leftTree of
          Just rightMost -> rightMost
          Nothing -> error "Unexpected behaviour while fetching rightMost"
        alteredLeftTree = removeRightMost leftTree

fetch :: Path -> BinTree a -> Maybe a
fetch _ Nil = Nothing
fetch [] (Node value _ _) = Just value
fetch (p : ps) (Node value leftTree rightTree) = case p of
  L -> fetch ps leftTree
  R -> fetch ps rightTree

fetchRightMost :: BinTree a -> Maybe a
fetchRightMost Nil = Nothing
fetchRightMost (Node value _ Nil) = Just value
fetchRightMost (Node _ _ rightTree) = fetchRightMost rightTree

removeRightMost :: BinTree a -> BinTree a
removeRightMost Nil = Nil
removeRightMost (Node _ leftTree Nil) = leftTree
removeRightMost (Node value leftTree rightTree) = Node value leftTree $ removeRightMost rightTree

exampleTree1 :: BinTree Int
exampleTree1 =
  Node
    1
    (Node 2 (Node 4 Nil Nil) Nil)
    (Node 3 (Node 5 Nil Nil) (Node 6 Nil Nil))

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
    ( Node
        'B'
        (Node 'D' Nil (Node 'J' Nil Nil))
        (Node 'H' Nil Nil)
    )
    ( Node
        'C'
        (Node 'E' Nil Nil)
        (Node 'F' Nil (Node 'I' Nil Nil))
    )

{-
         A
       /   \
      B     C
     / \   / \
    D   H E   F
     \         \
      J         I
 - -}

exampleTree3 :: BinTree Int
exampleTree3 =
  Node
    10
    ( Node
        5
        (Node 3 Nil Nil)
        (Node 7 Nil Nil)
    )
    ( Node
        15
        (Node 12 Nil Nil)
        (Node 18 Nil Nil)
    )

{-
        10
       /  \
      5    15
     / \   / \
    3   7 12 18
-}
