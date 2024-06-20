module Task1 where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

indices :: Text -> Text -> [Int]
indices needle = fmap (T.length . fst) . T.breakOnAll needle

calibrationValue :: Text -> Int
calibrationValue str = read [T.head digits, T.last digits]
  where
    digits = T.filter isDigit str

calibrationValueWithWords :: Text -> Int
calibrationValueWithWords t = read [head recons, last recons]
  where
    digits = [T.pack . show $ idx | idx <- [1 .. 9]]
    wordsAsdigits = T.pack <$> ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    digitIndices = liftA2 indices digits [t]
    wordIndices = liftA2 indices wordsAsdigits [t]
    totalIndices = zipWith (++) digitIndices wordIndices
    recons = snd <$> sort [(idx, head . show $ i) | (i, indexList) <- [1 ..] `zip` totalIndices, idx <- indexList]

main :: IO ()
main = do
  records <- T.lines <$> T.getContents

  let calibrationValues = calibrationValue <$> records

  putStrLn $ "Calibration value sum: " ++ show (sum calibrationValues)
  putStrLn $ "Sum of calibration values with digits from words: " ++ show (sum (calibrationValueWithWords <$> records))
