module Week4 where
import Data.List

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
fun1 = product . map (+(-2)) . filter even

-- Original implementation.
fun2test :: Integer -> Integer
fun2test 1 = 0
fun2test n | even n = n + fun2test (div n 2)
           | otherwise = fun2test (3 * n + 1)

-- Pointfree/idiomatic
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate mySeq

mySeq :: Integer -> Integer
mySeq x = if even x then div x 2 else 3 * x + 1

-- ----------------------------------------------------------------------------
-- Exercise 2.
-- Folding with trees.
-- ----------------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

--foldTree :: [a] -> Tree a
--foldTree list = foldr myFn Leaf list
-- Trees don't need to be sorted.  Just some binary function.
-- foldr on [a].
-- The function (a -> b -> b) for foldr is key.
-- Our zero is probably a Leaf?


-- The difficult part is starting with the zero at the root and building the
-- tree "downward", then the root node already has two leafs, and these need to
-- be replaced with actual nodes as we expand the foldr outward.


-- Sum powers of two up until the size of the input list. If you sum all
-- consecutive powers up to power N, then this equals to power (N+1)
-- Powers of 2: [1,2,4,8,16..]
-- If length list == 17, then we stop at 16 above. But we don't want 16 because
-- summing the powers of two including 16 is obviously more than 17. But we
-- know that all powers of 2 up until 16 summed together actually equal
-- sixteen, and this is definitely less than 17, so we just take everything but
-- the last element and this is the depth of our tree.
treeDepth :: [a] -> Int
treeDepth list = length $ init $ takeWhile (<= length list) $ map (2^) [0..]
