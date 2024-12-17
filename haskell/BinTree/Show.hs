module BinTree.Show where

import BinTree.Core
import Data.List

-- instance (Show a) => Show (BinTree a) where
--   show = prettyPrint

data ParentDir = PLeft | PRight | NoParent deriving (Show, Eq)
type ParentPos = Int
type Level = Int

dline = '|'
factor = 4

m c1 c2 = if c1 == dline then c1 else c2
zipWith' f xs [] = xs
zipWith' f [] xs = xs
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

buildLine pd a pp level = foldl (zipWith' m) "" ((((++ "|") . flip replicate ' ') . (factor *) <$> pp) ++ [replicate (factor * level) ' ' ++ cn ++ show a])
 where
  cn = case pd of
    PLeft -> "└──"
    PRight -> "┌──"
    NoParent -> "───"

prettyPrint' :: (Show a) => ParentDir -> [ParentPos] -> Level -> BinTree a -> [String]
prettyPrint' _ _ _ Nil = []
prettyPrint' pd pp level (Node a l r) =
  prettyPrint' PRight new_pp_r (level + 1) r
    ++ [buildLine pd a pp level]
    ++ prettyPrint' PLeft new_pp_l (level + 1) l
 where
  new_pp_r = case pd of
    PRight -> pp
    PLeft -> pp ++ [level]
    NoParent -> pp
  new_pp_l = case pd of
    PRight -> pp ++ [level]
    PLeft -> pp
    NoParent -> pp

prettyPrint t = putStrLn (intercalate "\n" (prettyPrint' NoParent [] 0 t))

--
-- prettyPrint :: (Show a) => BinTree a -> (Int, [String])
-- prettyPrint Nil = (0, [])
-- prettyPrint (Node val left right) = (nWidth, resultLines)
--  where
--   valStr = show val
--   valWidth = length valStr
--
--   (leftWidth, leftLines) = prettyPrint left
--
--   (rightWidth, rightLines) = prettyPrint right
--
--   nWidth = max (leftWidth + rightWidth + 1) valWidth
--
--   branchLine =
--     replicate leftWidth ' '
--       ++ (if leftWidth > 0 then "/ " else " ")
--       ++ replicate (nWidth - leftWidth - rightWidth - 1) ' '
--       ++ (if rightWidth > 0 then "\\" else " ")
--
--   subLines = zipLines leftLines rightLines
--
--   resultLines = [center valStr nWidth, branchLine] ++ subLines
--
-- center :: String -> Int -> String
-- center s width = replicate leftPad ' ' ++ s ++ replicate rightPad ' '
--  where
--   totalPad = width - length s
--   leftPad = totalPad `div` 2
--   rightPad = totalPad - leftPad
--
-- zipLines :: [String] -> [String] -> [String]
-- zipLines left right = [l ++ replicate gap ' ' ++ r | (l, r) <- zip left' right']
--  where
--   maxLines = max (length left) (length right)
--   left' =
--     if null left
--       then replicate maxLines ""
--       else left ++ replicate (maxLines - length left) (replicate (length (head left)) ' ')
--   right' =
--     if null right
--       then replicate maxLines ""
--       else right ++ replicate (maxLines - length right) (replicate (length (head right)) ' ')
--   gap = 2
