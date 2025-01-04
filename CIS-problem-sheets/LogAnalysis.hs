module LogAnalysis where

import Log
import Text.Read

type LogParser = (LogMessage, [String])

parseMessageType :: LogParser -> Maybe LogParser
parseMessageType (LogMessage _ ts s, e : n : rest) = case e of
    "E" -> case readMaybe n :: Maybe Int of
        Just n  -> Just (LogMessage (Error n) ts s, rest)
        Nothing -> Nothing
    "I" -> Just (LogMessage Info ts s, n : rest)
    "W" -> Just (LogMessage Warning ts s, n : rest)
    _   -> Nothing
parseMessageType _                                 = Nothing

parseTimestamp :: LogParser -> Maybe LogParser
parseTimestamp (LogMessage mt _ s, ts : rest) = 
    case readMaybe ts :: Maybe Int of
        Just n -> Just (LogMessage mt n s, rest)
        Nothing -> Nothing
parseTimestamp _                              = Nothing

parseMessageText :: LogParser -> Maybe LogParser
parseMessageText (LogMessage mt ts _, rest)
    | null rest = Nothing
    | otherwise = Just (LogMessage mt ts (unwords rest), [])

parseMessage :: String -> LogMessage
parseMessage s = case res of
    Just (logMsg, _) -> logMsg
    Nothing -> Unknown s

    where res = Just (LogMessage Info 0 "", words s) >>= 
                parseMessageType >>=
                parseTimestamp >>=
                parseMessageText

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf 
    = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lt lm'@(LogMessage _ ts' _) rt) 
    = case compare ts ts' of
        LT -> Node (insert lm lt) lm' rt
        GT -> Node lt lm' (insert lm rt)
insert (Unknown _) mt 
    = mt

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf              = []
inOrder (Node lt curr rt) = inOrder lt ++ [curr] ++ inOrder rt

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = [msg | (LogMessage mt ts msg) <- (inOrder . build) lms, 
                           case mt of (Error s) -> s >= 50; _ -> False]