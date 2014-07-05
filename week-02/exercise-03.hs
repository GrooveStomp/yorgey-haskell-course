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

