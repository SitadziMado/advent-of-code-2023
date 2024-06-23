{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Class (ask)
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parsing (int, lexeme, string')
import Text.Parsec.Char (char, space)
import Text.Parsec.Combinator (many1, sepBy, sepBy1)
import Text.Parsec.Prim (parse)
import Text.Parsec.Text (Parser)

data Card = Card {cardIndex :: !Int, winning :: !(Set Int), actual :: ![Int]} deriving (Show)

list :: Parser [Int]
list = many1 (lexeme int)

card :: Parser Card
card = do
  lexeme $ string' "Card"

  idx <- lexeme int

  lexeme $ string' ":"

  [win, act] <- sepBy1 (lexeme list) (lexeme $ char '|')

  pure Card {cardIndex = idx, winning = Set.fromList win, actual = act}

cardMatches :: Card -> Int
cardMatches Card {winning, actual} = length . filter (`Set.member` winning) $actual

cardCopyIndices :: Card -> [Int]
cardCopyIndices original@Card {cardIndex, winning, actual} = [cardIndex .. cardIndex - 1 + matches]
  where
    matches = cardMatches original

cardCopies :: Card -> Reader [Card] [Card]
cardCopies original = do
  cards <- ask

  pure $ (cards !!) <$> cardCopyIndices original

produceCopies :: [Card] -> [[Card]]
produceCopies cards = unfoldr generator cards
  where
    dup a = (a, a)
    copies original = runReader (cardCopies original) cards
    generator original = Just $ dup (original >>= copies)

main :: IO ()
main = do
  contents <- T.getContents

  let result = parse (many1 card) "Task4" contents
      cards =
        either
          ( \left -> do
              error "Wrong card format: " ++ show left
              []
          )
          id
          result
      cardPoints = (\k -> if k == 0 then 0 else 2 ^ (k - 1)) . cardMatches <$> cards

  putStrLn $ "Sum of the card points: " ++ show (sum cardPoints)

  let copies = takeWhile (not . null) . produceCopies $ cards

  putStrLn $ "Total card count: " ++ show ((length . concat) copies + length cards)