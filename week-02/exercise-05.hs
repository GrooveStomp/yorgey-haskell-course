{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parseMessage' :: [String] -> LogMessage
parseMessage' pieces
  | (=='I') label = LogMessage Info     timestampOrError  (unwords rest)
  | (=='W') label = LogMessage Warning  timestampOrError  (unwords rest)
  | (=='E') label = LogMessage (Error   timestampOrError) maybeTimestamp (unwords maybeRest)
  | otherwise = Unknown (unwords pieces)
  where
    label = head . head $ pieces
    timestampOrError = read . head . tail $ pieces
    rest = tail . tail $ pieces
    maybeTimestamp = read . head $ rest
    maybeRest = tail rest

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert newMsg@(LogMessage _ t1 _) (Node left msg@(LogMessage _ t2 _) right)
  | t1 < t2 = Node (insert newMsg left) msg right
  | otherwise = Node left msg (insert newMsg right)
insert _ tree@(Node _ (Unknown _) _) = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = inOrder' tree []

inOrder' :: MessageTree -> [LogMessage] -> [LogMessage]
inOrder' Leaf list = list
inOrder' (Node Leaf msg Leaf)  list = msg : list
inOrder' (Node Leaf msg right) list = msg : inOrder' right list
inOrder' (Node left msg Leaf)  list = inOrder' left (msg : list)
inOrder' (Node left msg right) list = inOrder' left (msg : inOrder' right list)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map messageFrom (filter (errorGreaterThan 50) sorted)
  where
    sorted = inOrder (build messages)

errorGreaterThan :: Int -> LogMessage -> Bool
errorGreaterThan n (LogMessage (Error z) _ _) = n < z
errorGreaterThan _ _ = False

messageFrom :: LogMessage -> String
messageFrom (LogMessage _ _ msg) = msg
messageFrom _ = ""
