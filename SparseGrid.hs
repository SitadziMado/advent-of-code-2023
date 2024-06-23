{-# LANGUAGE TupleSections #-}

module SparseGrid (SparseGrid, fromList, at, assocs, dfs, neighbors) where

import Control.Monad (unless)
import Control.Monad.RWS (RWS, asks, gets, modify, tell)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

newtype SparseGrid a = Grid {cells :: Map (Int, Int) a}

-- fromSizesPoints :: Int -> Int -> a -> [[a]] -> Grid a
-- fromSizesPoints w h d rows = Grid { width = w, height = h, cells = Map.fromAscList withCoords}
--   where withCoords = [((j, i), cell) | (row, i) <- (rows ++ repeat []) `zip` [0..(h - 1)]
--                                      , (cell, j) <- (row ++ repeat d) `zip` [0..(w - 1)]]

fromList :: [((Int, Int), a)] -> SparseGrid a
fromList = Grid . Map.fromList

at :: (Int, Int) -> SparseGrid a -> Maybe a
at point = Map.lookup point . cells

assocs :: SparseGrid a -> [((Int, Int), a)]
assocs = Map.assocs . cells

add :: Num a => (a, a) -> (a, a) -> (a, a)
add a = uncurry bimap (bimap (+) (+) a)

neighbors :: Num a => a -> a -> [(a, a)]
neighbors x y = [(x, y) `add` vec | vec <- fourSides east ++ fourSides northEast]
  where
    east = (1, 0)
    northEast = (1, 1)
    rotate x y = (y, - x)
    fourSides = take 4 . iterate (uncurry rotate)

dfs :: (Int, Int) -> RWS (SparseGrid a) [((Int, Int), a)] (Set (Int, Int)) ()
dfs point = do
  been <- gets $ Set.member point

  unless been $ do
    value <- asks $ SparseGrid.at point
    tell . pure $ (point, fromJust value)

    let closest = uncurry neighbors point

    cells <- asks $ (<*>) (SparseGrid.at <$> closest) . pure

    let assocs = zipWith (\a -> fmap (,a)) closest cells

    modify $ Set.insert point
    sequence_ $ dfs . snd <$> catMaybes assocs
