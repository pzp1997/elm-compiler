module Data.MultiSet where

import qualified Data.Map as Map

type MultiSet a = Map.Map a Int

empty :: MultiSet a
empty = Map.empty

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert = Map.alter (\x -> case x of Nothing -> Just 1; Just n -> Just $ n + 1)

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union = Map.unionWith (+)

unions :: Ord a => [MultiSet a] -> MultiSet a
unions = foldr union Map.empty

difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference = Map.unionWith (\x y -> max (x - y) 0)

singleton :: Ord a => a -> MultiSet a
singleton x = Map.singleton x 1

fromMap :: Map.Map a Int -> MultiSet a
fromMap x = x

toMap :: Ord a => MultiSet a -> Map.Map a Int
toMap = Map.filter (> 0)

fold :: Ord b => (a -> b -> a) -> a -> MultiSet b -> a
fold f = Map.foldlWithKey (\acc key _ -> f acc key)
