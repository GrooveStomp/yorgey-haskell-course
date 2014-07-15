module Week4 where

-- ----------------------------------------------------------------------------
-- Exercise 1.
-- Wholemeal Programming.
-- ----------------------------------------------------------------------------

-- Original implementation.
fun1test :: [Integer] -> Integer
fun1test [] = 1
fun1test (x:xs)
  | even x = (x - 2) * fun1test xs
  | otherwise = fun1test xs

-- Pointfree/idiomatic
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

-- Original implementation.
fun2test :: Integer -> Integer
fun2test 1 = 0
fun2test n | even n = n + fun2test (n `div` 2)
           | otherwise = fun2test (3 * n + 2)

-- Pointfree/idiomatic
--fun2 :: Integer -> Integer
--fun2 = sum . map (\x -> 
