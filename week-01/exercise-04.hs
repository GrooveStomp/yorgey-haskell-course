import Data.Char

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = map fromIntegral $ map digitToInt $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse . toDigits $ n

doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x:y:zs) = [x,y*2] ++ doubleSecond zs
doubleSecond [x] = [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse . doubleSecond . reverse $ list

sumDigits :: [Integer] -> Integer
sumDigits nums = foldl (+) 0 $ concat . map toDigits $ nums

validate :: Integer -> Bool
validate n = (==0) $ (sumDigits . concat . map toDigits . doubleEveryOther . toDigits $ n) `mod` 10
