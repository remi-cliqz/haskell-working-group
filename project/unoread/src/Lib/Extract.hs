
module Lib.Extract where

import Lib.Prelude
import Lib.Types.Article hiding (url, content)
import Text.HTML.TagSoup (parseTags, Tag(..), isTagText, innerText)
-- import qualified Data.ByteString.Lazy as LBS

extract :: LText -> LByteString -> (Article, LByteString)
extract url content =
    let ltextUrl = Url url
        tags = parseTags content
     in (Article ltextUrl $ Content ("" :: LText), innerText tags)
     -- filter (\(TagText lbs) -> LBS.length lbs > 100) . filter isTagText
