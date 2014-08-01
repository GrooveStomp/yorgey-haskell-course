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

construct :: a -> Tree a -> Tree a
construct a Leaf = Node 0 Leaf a Leaf
construct a (Node _ Leaf        root Leaf        ) = Node 1 (Node 0 Leaf a Leaf) root Leaf
construct a (Node c left@Node{} root Leaf        ) = Node c left                 root (Node 0 Leaf a Leaf)
construct a (Node c Leaf        root right@Node{}) = Node c (Node 0 Leaf a Leaf) root right

construct a (Node c left@(Node leftCount ll lt lr) root right@(Node rightCount rl rt rr))
  | freeSpace left         = Node c     (construct a left) root right
  | freeSpace right        = Node c     left               root (construct a right)
  | leftCount < rightCount = Node c     (construct a (Node (leftCount+1) ll lt lr)) root right
  | leftCount > rightCount = Node c     left               root (construct a (Node (rightCount+1) rl rt rr))
  | otherwise              = Node (c+1) (construct a left) root right

freeSpace :: Tree a -> Bool
freeSpace (Node _ (Node _ Leaf _ Leaf) _ (Node _ Leaf _ Leaf)) = False
freeSpace (Node _ Leaf                 _ Leaf                ) = False
freeSpace (Node _ Leaf                 _ Node{}              ) = True
freeSpace (Node _ Node{}               _ Leaf                ) = True
freeSpace (Node _ left@Node{}          _ right@Node{}        ) = freeSpace left || freeSpace right

foldTree :: [a] -> Tree a
foldTree = foldr construct Leaf
