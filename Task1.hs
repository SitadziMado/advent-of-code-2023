import Data.Char (isDigit)

calibrationValue :: String -> Int
calibrationValue str = read [head digits, last digits]
  where digits = filter isDigit str

main :: IO ()
main = do
  records <- lines <$> getContents

  let calibrationValues = calibrationValue <$> records

  putStrLn $ "Calibration value sum: " ++ show (sum calibrationValues)
