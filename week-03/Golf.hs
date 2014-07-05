module Golf where

skips :: [a] -> [[a]]
-- get the size of the list
-- for size to zero:
--   generate a new list that is every nth element of the first list
-- collect all lists into an array
skips list = 

indices :: Int -> Int -> [Int]
indices n len = [x*n|x <- [0..len], x*n < len]

fromList :: [a] -> [Int] -> [a]
fromList = map . (!!)
