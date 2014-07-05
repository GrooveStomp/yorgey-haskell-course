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
