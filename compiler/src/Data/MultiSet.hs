{-# LANGUAGE FlexibleContexts #-}
module Data.MultiSet where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

newtype MultiSet a = MultiSet (Map.Map a Int)

empty :: MultiSet a
empty = MultiSet Map.empty

(!) :: Ord a => MultiSet a -> a -> Int
(!) (MultiSet s) x = fromMaybe 0 $ Map.lookup x s

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet s) = MultiSet $
  Map.alter (\x -> case x of Nothing -> Just 1; Just n -> Just $ n + 1) x s

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

fold :: Ord b => (a -> b -> a) -> a -> MultiSet b -> a
fold f base (MultiSet s) = Map.foldlWithKey (\acc key _ -> f acc key) base s

instance Ord a => Semigroup (MultiSet a) where
  (<>) = union
