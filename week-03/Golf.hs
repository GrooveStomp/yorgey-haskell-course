module Golf where

-- Exercise 1.
-- Hopscotch
skips :: [a] -> [[a]]
skips list = map (\x -> map (list !!) [x-1,x*2-1..listLength-1]) [1..listLength]
  where
    listLength = length list

-- Exercise 2
-- Local Maxima
-- let list = [1,2,3,4,5]

-- split the input list into overlapping partitions of size 3.
partitions' :: [a] -> [[a]]
partitions' list = map (\x -> take 3 (drop x list) ) [0..length list - 3]

