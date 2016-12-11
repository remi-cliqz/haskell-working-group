{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Printf
import Protolude
import Lib


main :: IO ()
main = do
    [rawUrl] <- getArgs
    result <- fetch rawUrl
    case result of
      Left e -> printf "Unable to fetch URL %s (%s)\n" rawUrl e
      Right lbs -> putStrLn $ extract (toS rawUrl) lbs
