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

doubleEveryOtherForward :: [Integer] -> [Integer]
doubleEveryOtherForward [] = []
doubleEveryOtherForward (x:y:zs) = [x,y*2] ++ doubleEveryOtherForward zs
doubleEveryOtherForward [x] = [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse .doubleEveryOtherForward . reverse $ list

sumDigits :: [Integer] -> Integer
sumDigits nums = foldl (+) 0 $ concat . map toDigits $ nums
