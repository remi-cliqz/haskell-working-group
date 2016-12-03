{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Fetch (fetch, FetchException, HttpException) where

import Lib.Prelude
import Network.HTTP.Req
import Data.Text (Text, stripPrefix)


data FetchException = WrongProtocol deriving (Show, Typeable)
instance Exception FetchException

instance MonadHttp IO where
    handleHttpException = throwIO

fetch :: Text -> IO LByteString
fetch url =
  case "https://" `stripPrefix` url of
    Just hostname -> go https hostname
    Nothing ->
      case "http://" `stripPrefix` url of
        Just hostname -> go http hostname
        Nothing -> throwIO WrongProtocol
    where
        go protocol hostname = do
            lbs <- req GET (protocol hostname) NoReqBody lbsResponse mempty
            return (responseBody lbs)
