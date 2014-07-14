module Golf where

-- ----------------------------------------------------------------------------
-- Exercise 1.
-- Hopscotch
-- ----------------------------------------------------------------------------
skips :: [a] -> [[a]]
skips list = map (\x -> map (list !!) [x-1,x*2-1..listLength-1]) [1..listLength]
  where
    listLength = length list

-- ----------------------------------------------------------------------------
-- Exercise 2
-- Local Maxima
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
