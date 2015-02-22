{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line = parseLine
  where
    tokens = words line

    messageType :: Maybe MessageType
    messageType = let level = head tokens
                      sev   = read . head . drop 1 $ tokens
                  in case level of
                    "E" -> Just (Error sev)
                    "I" -> Just Info
                    "W" -> Just Warning
                    _   -> Nothing

    timestamp :: TimeStamp
    timestamp = read . head . drop howMany $ tokens
      where
        howMany = case messageType of Just (Error _) -> 2
                                      _              -> 1

    message :: String
    message = unwords . drop howMany $ tokens
      where
        howMany = case messageType of Just (Error _) -> 3
                                      Just _         -> 2
                                      Nothing        -> 0

    parseLine =
        case messageType of Just (Error sev) -> LogMessage (Error sev) timestamp message
                            Just Info        -> LogMessage Info timestamp message
                            Just Warning     -> LogMessage Warning timestamp message
                            Nothing          -> Unknown message

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)