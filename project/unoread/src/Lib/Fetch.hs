{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Lib.Fetch (fetch) where

-- USING curl
import Lib.Prelude
import Network.Curl.Download.Lazy


fetch :: String -> IO (Either String LByteString)
fetch = openLazyURI

-- USING HTTP
-- import Lib.Prelude
-- import Network.HTTP
--
-- type String = [Char]
--
-- fetch :: String -> IO (Either SomeException String)
-- fetch url = try $ simpleHTTP (getRequest url) >>= getResponseBody

-- USING req
-- import Lib.Prelude hiding (get)
-- import Network.HTTP.Req
--
--
-- data FetchingResult =
--       Exception HttpException
--     | Result LByteString
--     | WrongUrl
--     deriving (Show)
--
--
-- instance MonadHttp IO where
--     handleHttpException = throwIO
--
--
-- -- Low-level fetch function that may throw an exception
-- unsafeFetch :: Url scheme -> Option scheme -> IO LByteString
-- unsafeFetch url options = do
--     lbs <- req GET url NoReqBody lbsResponse options
--     return $ responseBody lbs
--
--
-- -- Wrap unsafeFetch in a `try` to expose a type-safe API
-- safeFetch :: Url scheme -> Option scheme -> IO FetchingResult
-- safeFetch url options = fmap (either Exception Result) . try $ unsafeFetch url options
--
--
-- -- Handle HTTP/HTTPs fetching as well as parsing of the URL
-- fetch :: ByteString -> IO FetchingResult
-- fetch rawUrl
--   | Just (url, options) <- parseUrlHttp rawUrl  = safeFetch url options
--   | Just (url, options) <- parseUrlHttps rawUrl = safeFetch url options
--   | otherwise                                   = return WrongUrl
