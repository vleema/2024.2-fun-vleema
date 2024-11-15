module BinTree.Core where

data BinTree a where
  Node :: a -> Maybe (BinTree a) -> Maybe (BinTree a) -> BinTree a

data Direction where
  L :: Direction
  R :: Direction
  deriving (Show)

type Path = [Direction]

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
levelOrder binTree = concat [showLevel level binTree | level <- [0 .. height binTree]]

showLevel :: (Show a) => Int -> BinTree a -> String
showLevel 0 (Node value _ _) = show value
showLevel n (Node _ leftTree rightTree) = maybe "" (showLevel (n - 1)) leftTree ++ maybe "" (showLevel (n - 1)) rightTree

height :: BinTree a -> Int
height (Node _ leftTree rightTree) = 1 + max (maybe 0 height leftTree) (maybe 0 height rightTree)

search :: (Ord a) => a -> BinTree a -> Maybe Path
search value (Node rootValue leftTree rightTree)
  | value == rootValue = Just []
  | value > rootValue = addPath R rightTree
  | value < rootValue = addPath L leftTree
 where
  addPath direction subtree =
    (direction :) <$> (search value =<< subtree)

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert value (Node rootValue leftTree rightTree)
  | value == rootValue = Node rootValue leftTree rightTree
  | value > rootValue = case leftTree of
      Nothing -> Node rootValue (Just (Node value Nothing Nothing)) rightTree
      Just leftTree -> insert value leftTree
  | value < rootValue = case rightTree of
      Nothing -> Node rootValue leftTree (Just (Node value Nothing Nothing))
      Just rightTree -> insert value rightTree

remove :: (Ord a) => a -> BinTree a -> Maybe (BinTree a)
remove value (Node rootValue leftTree rightTree)
  | value > rootValue = Just $ Node rootValue leftTree $ remove value =<< rightTree
  | value < rootValue = Just $ Node rootValue (remove value =<< leftTree) rightTree
  | value == rootValue = case (leftTree, rightTree) of
      (leftTree, Nothing) -> leftTree
      (Nothing, rightTree) -> rightTree
      (Just leftTree, rightTree) -> Just $ Node leftMaximum alteredLeftTree rightTree
       where
        leftMaximum = fetchRightMost leftTree
        alteredLeftTree = removeRightMost leftTree

fetch :: Path -> BinTree a -> Maybe a
fetch [] (Node value _ _) = Just value
fetch (p : ps) (Node value leftTree rightTree) = case p of
  L -> fetch ps =<< leftTree
  R -> fetch ps =<< rightTree

fetchRightMost :: BinTree a -> a
fetchRightMost (Node value _ Nothing) = value
fetchRightMost (Node _ _ (Just rightTree)) = fetchRightMost rightTree

removeRightMost :: BinTree a -> Maybe (BinTree a)
removeRightMost (Node _ leftTree Nothing) = leftTree
removeRightMost (Node value leftTree (Just rightTree)) = Just $ Node value leftTree $ removeRightMost rightTree

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
    ( Just
        ( Node
            5
            (Just (Node 3 Nothing Nothing))
            (Just (Node 7 Nothing Nothing))
        )
    )
    ( Just
        ( Node
            15
            (Just (Node 12 Nothing Nothing))
            (Just (Node 18 Nothing Nothing))
        )
    )

{-
        10
       /  \
      5    15
     / \   / \
    3   7 12 18
-}
