import Data.Char

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = map fromIntegral $ map digitToInt $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = map fromIntegral $ map digitToInt $ reverse . show $ n
