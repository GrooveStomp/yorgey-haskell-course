module Golf where

skips :: [a] -> [[a]]
skips list = map (\x -> map (list !!) [x-1,x*2-1..listLength-1]) [1..listLength]
  where
    listLength = length list
