{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude
import Lib
import Control.Exception (displayException)


main :: IO ()
main = do
    [rawUrl] <- getArgs
    result <- fetch (toS rawUrl)
    case result of
      WrongUrl -> putText "Could not parse URL (It should either start by http:// or https://)"
      Exception e -> do
          putText "An exception was raised while fetching"
          putStrLn . displayException $ e
      Result lbs -> putStrLn . snd $ extract (toS rawUrl) lbs
