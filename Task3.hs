{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SparseGrid (SparseGrid)
import qualified SparseGrid
import Text.Parsec.Char (anyChar, char, digit, newline)
import Text.Parsec.Combinator (manyTill,many1)
import Text.Parsec.Prim (parse)
import Text.Parsec.Text (Parser)
import Control.Monad.RWS (execRWS)
import Data.Tuple (swap)
import Data.List ( sortBy )

data Cell = Symbol Char | Digit Char deriving Show

isDigit :: Cell -> Bool
isDigit (Digit ch) = True
isDigit _ = False

cellChar :: Cell -> Char
cellChar (Symbol ch) = ch
cellChar (Digit ch) = ch

cell :: Parser (Maybe Cell)
cell = (char '.' >> pure Nothing) <|> (Just . Digit <$> digit) <|> (Just . Symbol <$> anyChar)

row :: Parser [(Int, Cell)]
row = do
  cells <- manyTill cell newline

  pure $ catMaybes [fmap (j,) a | (j, a) <- [0 ..] `zip` cells]

grid :: Parser (SparseGrid Cell)
grid = do
  rows <- many1 row

  pure $ SparseGrid.fromList [((j, i), a) | (i, b) <- [0 ..] `zip` rows, (j, a) <- b]

groupAdjacentBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjacentBy p = foldr f []
  where f a ((x:xs):acc)
          | p a x = (a:x:xs):acc
          | otherwise = [a]:(x:xs):acc
        f a [] = [[a]]
        f _ _ = error "Should not happen"

main :: IO ()
main = do
  contents <- T.getContents

  let g = either (error "Failed to parse grid") id (parse grid "Task3.Main" contents)
      symbolCoords = fmap fst . filter (not . isDigit . snd) . SparseGrid.assocs $ g


  let pass computation = execRWS computation g Set.empty
      paths = snd . pass . SparseGrid.dfs <$> symbolCoords
      digitCoords = fst <$> filter (isDigit . snd) (concat paths)
      ordered = sortBy (\a b -> swap a `compare` swap b) digitCoords
      groups = groupAdjacentBy (\(x1, y1) (x2, y2) -> y1 == y2 && abs (x2 - x1) <= 1) ordered
      nums = read <$> filter (not . null) [catMaybes [cellChar <$> SparseGrid.at digit g| digit <- grp] | grp <- groups] :: [Int]

  print groups
  print nums
  print $ sum nums