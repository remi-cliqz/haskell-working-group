
module Lib.Extract where

import Lib.Prelude
-- import Lib.Types.Article hiding (url, content)
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isPunctuation, isSymbol, isSpace)


isEmpty :: LByteString -> Bool
isEmpty lbs = LBS.length lbs == 0 || C.all isSpace lbs


countSpecial :: LByteString -> Int
countSpecial = length . C.findIndices (\c -> isPunctuation c || isSymbol c)

ratio :: LByteString -> Double
ratio lbs = let nPunc = fromIntegral . countSpecial $ lbs
                nTotal = fromIntegral . LBS.length $ lbs
             in nPunc / nTotal

extract :: LText -> LByteString -> LByteString
extract _ content =
    let tags = map fromTagText . filter isTagText . parseTags $ content
 in C.unlines . filter (\t -> ratio t < 0.1 && not (isEmpty t)) $ tags
