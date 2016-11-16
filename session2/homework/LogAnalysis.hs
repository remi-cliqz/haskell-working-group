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

parseMessage :: String -> LogMessage
parseMessage = undefined


parse :: String -> [LogMessage]
parse = undefined

-- rest for ya!
