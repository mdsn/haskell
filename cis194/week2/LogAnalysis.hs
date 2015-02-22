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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ timestamp _) tree =
    case tree of
        Leaf -> Node Leaf m Leaf
        Node left node right ->
            case node of
                (Unknown _) -> Node left node right
                (LogMessage _ time _) -> if timestamp < time then Node (insert m left) node right
                                                             else Node left node (insert m right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build ms = foldr insert Leaf ms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = left ++ [x] ++ right
  where
    left = inOrder l
    right = inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ms = map message . filter severe . inOrder . build $ ms
  where
    severe :: LogMessage -> Bool
    severe (LogMessage (Error severity) _ _) = severity >= 50
    severe _                                 = False

    message :: LogMessage -> String
    message (LogMessage _ _ m) = m
    message _                  = ""