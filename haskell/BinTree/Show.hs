module BinTree.Show where

import BinTree.Core

instance (Show a) => Show (BinTree a) where
  show tree = unlines $ snd $ prettyPrint tree

prettyPrint :: (Show a) => BinTree a -> (Int, [String])
prettyPrint (Node val left right) = (nWidth, resultLines)
 where
  valStr = show val
  valWidth = length valStr

  (leftWidth, leftLines) = maybe (0, []) prettyPrint left

  (rightWidth, rightLines) = maybe (0, []) prettyPrint right

  nWidth = max (leftWidth + rightWidth + 1) valWidth

  branchLine =
    replicate leftWidth ' '
      ++ (if leftWidth > 0 then "/ " else " ")
      ++ replicate (nWidth - leftWidth - rightWidth - 1) ' '
      ++ (if rightWidth > 0 then "\\" else " ")

  subLines = zipLines leftLines rightLines

  resultLines = [center valStr nWidth, branchLine] ++ subLines

center :: String -> Int -> String
center s width = replicate leftPad ' ' ++ s ++ replicate rightPad ' '
 where
  totalPad = width - length s
  leftPad = totalPad `div` 2
  rightPad = totalPad - leftPad

zipLines :: [String] -> [String] -> [String]
zipLines left right = [l ++ replicate gap ' ' ++ r | (l, r) <- zip left' right']
 where
  maxLines = max (length left) (length right)
  left' =
    if null left
      then replicate maxLines ""
      else left ++ replicate (maxLines - length left) (replicate (length (head left)) ' ')
  right' =
    if null right
      then replicate maxLines ""
      else right ++ replicate (maxLines - length right) (replicate (length (head right)) ' ')
  gap = 2
