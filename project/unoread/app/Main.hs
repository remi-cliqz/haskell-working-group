{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (Handler(..))
import Protolude
import Lib


main :: IO ()
main = do
    lbs <- fetch "https://google.fr" `catches` [Handler (\ (ex :: HttpException) -> return "HttpException" :: IO LByteString),
                                                Handler (\ (ex :: FetchException) -> return "FetchException" :: IO LByteString)]
    print lbs
