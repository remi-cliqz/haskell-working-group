
module Lib.Types.Article
    ( Article(..)
    , Url(..)
    , Content(..)
    ) where

import Lib.Prelude (LText, Eq, Show)

newtype Url = Url LText deriving (Show, Eq)
newtype Content = Content LText deriving (Show, Eq)

data Article = Article
    { url :: Url
    , content :: Content
    } deriving (Eq, Show)
