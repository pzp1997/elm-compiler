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

-- put :: Ord a => a -> Int -> MultiSet a -> MultiSet a
-- put x n (MultiSet m) = MultiSet $ Map.insert x n m

insertMany :: Ord a => a -> Int -> MultiSet a -> MultiSet a
insertMany x n (MultiSet m) = MultiSet $ Map.insertWith (+) x n m

removeOne :: Ord a => a -> MultiSet a -> MultiSet a
removeOne x m = removeMany x 1 m

removeMany :: Ord a => a -> Int -> MultiSet a -> MultiSet a
removeMany x n' (MultiSet m) = MultiSet $ Map.adjust (\n -> n - n') x m

removeAll :: Ord a => a -> MultiSet a -> MultiSet a
removeAll x (MultiSet m) = MultiSet $ Map.delete x m

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
  Map.foldlWithKey (\acc x n -> if n > 0 then Set.insert x acc else acc) Set.empty s

fold :: Ord b => (a -> b -> a) -> a -> MultiSet b -> a
fold f base (MultiSet s) = Map.foldlWithKey (\acc key _ -> f acc key) base s

filter :: Ord a => (a -> Bool) -> MultiSet a -> MultiSet a
filter f (MultiSet s) = MultiSet $ Map.filterWithKey (\k _ -> f k) s

foldlWithKey :: (a -> k -> Int -> a) -> a -> MultiSet k -> a
foldlWithKey f base (MultiSet s) = Map.foldlWithKey f base s

foldrWithKey :: (k -> Int -> b -> b) -> b -> MultiSet k -> b
foldrWithKey f base (MultiSet s) = Map.foldrWithKey f base s


instance Ord a => Semigroup (MultiSet a) where
  (<>) = union
