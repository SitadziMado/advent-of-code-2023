{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.List (groupBy, maximumBy, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parsing (int, lexeme, string')
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Prim (parse)
import Text.Parsec.Text (Parser)

data ColorTag = Red | Green | Blue deriving (Eq, Ord, Show)

type Draw = [(ColorTag, Int)]

data Game = Game {idx :: Int, draws :: [Draw]} deriving (Show)

tag :: Parser ColorTag
tag = (string' "red" >> pure Red) <|> (string' "green" >> pure Green) <|> (string' "blue" >> pure Blue)

cubes :: Parser (ColorTag, Int)
cubes = do
  count <- lexeme int
  tag <- lexeme tag

  pure (tag, count)

draw :: Parser Draw
draw = sepBy1 (lexeme cubes) (lexeme $ char ',')

game :: Parser Game
game = do
  lexeme $ string' "Game"
  index <- lexeme int
  lexeme $ char ':'

  draws <- sepBy1 (lexeme draw) (lexeme $ char ';')

  pure $ Game {idx = index, draws = draws}

isCubeLegal :: (Ord a, Num a) => (ColorTag, a) -> Bool
isCubeLegal (Red, n) = n <= 12
isCubeLegal (Green, n) = n <= 13
isCubeLegal (Blue, n) = n <= 14

isLegal :: (Foldable t, Functor t) => (a -> Bool) -> t a -> Bool
isLegal p = all (== True) . fmap p

isDrawLegal :: Draw -> Bool
isDrawLegal = isLegal isCubeLegal

isGameLegal :: Game -> Bool
isGameLegal Game {draws} = isLegal isDrawLegal draws

fewestCubes :: Game -> [(ColorTag, Int)]
fewestCubes Game {draws} = maximumBy (\(_, a) (_, b) -> a `compare` b) <$> summary
  where
    summary = groupBy (\a b -> fst a == fst b) . sort . concat $ draws

main :: IO ()
main = do
  records <- T.lines <$> T.getContents

  let (errors, games) = partitionEithers $ parse game "" <$> records
      legalGames = filter isGameLegal games

  unless
    (null errors)
    $ do
      putStr "We encountered the following errors while parsing: "
      sequence_ $ print <$> errors

  putStrLn $ "Legal game index sum: " ++ show (sum $ idx <$> legalGames)

  let powers = [product $ snd <$> sufficientCount | sufficientCount <- fewestCubes <$> games]

  putStrLn $ "The sum of powers of the cube sets is: " ++ show (sum powers)
