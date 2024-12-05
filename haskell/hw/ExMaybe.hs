module ExMaybe where

-- Do not alter this import!

import Data.Maybe qualified as M
import Prelude hiding (Maybe (..), maybe)

data Maybe a = Nothing | Just a
  deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m : ms) =
  case m of
    Nothing -> catMaybes ms
    Just a -> a : catMaybes ms

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Kaboom!"

fromMaybe :: a -> Maybe a -> a
fromMaybe default_ Nothing = default_
fromMaybe default_ (Just a) = a

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap _ [] = []
justMap f (a : as) =
  case f a of
    Nothing -> justMap f as
    Just a -> a : justMap f as

maybe :: b -> (a -> b) -> Maybe a -> b
maybe default_ _ Nothing = default_
maybe _ f (Just a) = f a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [x] = Just x
listToMaybe (x : _) = error "Kaboom!"

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith (mf : mfs) (a : as) =
  case mf of
    Nothing -> tryToModifyWith mfs as
    Just f -> f a : tryToModifyWith mfs as
tryToModifyWith _ _ = []
