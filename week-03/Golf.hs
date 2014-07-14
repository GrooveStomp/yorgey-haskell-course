module Golf where
import Data.Tuple
import Data.List

-- ----------------------------------------------------------------------------
-- Exercise 1.
-- Hopscotch.
-- ----------------------------------------------------------------------------
skips :: [a] -> [[a]]
skips list = map (\x -> map (list !!) [x-1,x*2-1..listLength-1]) [1..listLength]
  where
    listLength = length list

-- ----------------------------------------------------------------------------
-- Exercise 2.
-- Local Maxima.
-- ----------------------------------------------------------------------------

-- split the input list into overlapping partitions of size 3.
partitions :: [a] -> [[a]]
partitions list = map (\x -> take 3 (drop x list) ) [0..length list - 3]

-- Return true if the middle Integer is largest.
-- [Integer] is incorrect - we actually only allow triples.
midMax :: [Integer] -> Bool
midMax list = list !! 1 > list !! 0 && list !! 1 > list !! 2

-- Return the middle element, aka the maxima.
-- If the middel element isn't the largest, then return [].
maxima :: [Integer] -> [Integer]
maxima list
  | midMax list = [list !! 1]
  | otherwise = []

-- This is the function to implement for the exercise.
localMaxima :: [Integer] -> [Integer]
localMaxima list = foldl (\x y -> (++) x $ maxima y) [] $ partitions list

-- ----------------------------------------------------------------------------
-- Exercise 3.
-- Histogram.
-- ----------------------------------------------------------------------------
histogram :: [Integer] -> String
histogram list = "\n" ++ (intercalate "\n" (histogram' list)) ++ "\n==========\n0123456789\n\n"

histogram' :: [Integer] -> [[Char]]
histogram' list = map (\x -> map toHistogramMarker x) $ frequencyChart (frequencyList list) []

toHistogramMarker :: Bool -> Char
toHistogramMarker True = '*'
toHistogramMarker False = ' '

-- Given a list of integers "list" and an integer "num", return the number of
-- occurrences of num in list.
numOccurrences :: [Integer] -> Integer -> Integer
numOccurrences list num = toInteger $ length $ filter (\x -> (==num) x) list

-- Simply map 'numOccurrences' across the integer list [0,1,2,3,4,5,6,7,8,9]
frequencyList :: [Integer] -> [Integer]
frequencyList list = map (numOccurrences list) [0..9]

-- Given a list of integers, return two lists:
-- The same list, but with every element decremented by one,
-- and the same list, but with trues where the value was greater than zero.
decrPresence :: [Integer] -> ([Integer], [Bool])
decrPresence list = (map (\x -> x - 1) list, map (> 0) list)

areAnyPresent :: [Integer] -> Bool
areAnyPresent list = elem True $ map (> 0) list

frequencyChart :: [Integer] -> [[Bool]] -> [[Bool]]
frequencyChart list chart
  | not $ areAnyPresent list = chart
  | otherwise = frequencyChart newList $ [presences] ++ chart
    where
      pair = decrPresence list
      newList = fst pair
      presences = snd pair
