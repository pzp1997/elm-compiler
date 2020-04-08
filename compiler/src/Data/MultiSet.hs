{-# LANGUAGE FlexibleContexts #-}
module Data.MultiSet where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype MultiSet a = MultiSet (Map.Map a Int)
  deriving (Eq, Show)

empty :: MultiSet a
empty = MultiSet Map.empty

(!) :: Ord a => MultiSet a -> a -> Int
(!) (MultiSet s) x = fromMaybe 0 $ Map.lookup x s

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet s) = MultiSet $ Map.insertWith (+) x 1 s

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet s) (MultiSet t) = MultiSet $ Map.unionWith (+) s t

unions :: Ord a => [MultiSet a] -> MultiSet a
unions = foldr union empty

difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet s) (MultiSet t) = MultiSet $ Map.unionWith (\x y -> x - y) s t

singleton :: Ord a => a -> MultiSet a
singleton x = MultiSet $ Map.singleton x 1

fromMap :: Map.Map a Int -> MultiSet a
fromMap = MultiSet

toMap :: Ord a => MultiSet a -> Map.Map a Int
toMap (MultiSet s) = Map.filter (> 0) s

toSet :: Ord a => MultiSet a -> Set.Set a
toSet (MultiSet s) =
    Map.foldlWithKey (\acc x n ->
        if n > 0 then Set.insert x acc
        else acc
    ) Set.empty s

fold :: Ord b => (a -> b -> a) -> a -> MultiSet b -> a
fold f base (MultiSet s) = Map.foldlWithKey (\acc key _ -> f acc key) base s

filter :: Ord a => (a -> Bool) -> MultiSet a -> MultiSet a
filter f (MultiSet s) = MultiSet $ Map.filterWithKey (\k _ -> f k) s

instance Ord a => Semigroup (MultiSet a) where
  (<>) = union
