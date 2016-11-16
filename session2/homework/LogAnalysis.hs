{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
parseMessage "E 2 562 help help"
== LogMessage (Error 2) 562 "help help"
2cis 194: homework 2
parseMessage "I 29 la la la"
== LogMessage Info 29 "la la la"
parseMessage "This is not in the right format"
== Unknown "This is not in the right format"
-}
{-
parseMType :: [String] -> Maybe MessageType
parseMType [] = Nothing
parseMType (x:xs) =
  case x of
    "I" -> Just Info
    "W" -> Just Warning
    "E" -> Just (Error (read $ head xs))
    _  -> Nothing


parseMessage :: String -> LogMessage
parseMessage m =  case etype of
                    Nothing -> Unknown m
                    Just some -> case some of
                      Error ecode -> LogMessage (Error ecode) (read $ head $ tail $ tail wm) m
                      s -> LogMessage s (read $ head $ tail wm) m
                      
  where
    wm = words m
    etype = parseMType wm
-}

parseMessage2 :: String -> LogMessage
parseMessage2 m = go (words m)
  where
    go ("I":t:xs) = LogMessage Info (read t) (unwords xs)
    go ("W":t:xs) = LogMessage Warning (read t) (unwords xs)
    go ("E":s:t:xs) = LogMessage (Error (read s)) (read t) (unwords xs)
    go _ = Unknown m
    
parse :: String -> [LogMessage]
parse = undefined

-- rest for ya!

