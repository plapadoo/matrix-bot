module Plpd.Util
  ( textShow
  , formatStrict
  , surround
  , none
  , surroundHtml
  , surroundQuotes
  , breakOnMaybe
  , textHashAsText
  ) where

import           Crypto.Hash             (Digest, SHA3_512, hash)
import           Data.Bool               (Bool, not)
import           Data.ByteString         (ByteString)
import           Data.Foldable           (Foldable, any)
import           Data.Function           ((.))
import           Data.Maybe              (Maybe (..))
import           Data.Monoid             (Monoid, (<>))
import           Data.Text               (Text, breakOn, pack)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import           Data.Text.Lazy          (toStrict)
import           Text.Show               (Show, show)

formatStrict
  :: Params ps
  => Format -> ps -> Text
formatStrict a b = toStrict (format a b)

textHashAsText :: Text -> Text
textHashAsText =
  pack . show . (hash :: ByteString -> Digest SHA3_512) . encodeUtf8

textShow
  :: Show a
  => a -> Text
textShow = pack . show

surround
  :: Monoid m
  => m -> m -> m -> m
surround before after inside = before <> inside <> after

surroundHtml :: Text -> Text -> Text
surroundHtml tagName inside =
  "<" <> tagName <> ">" <> inside <> "</" <> tagName <> ">"

surroundQuotes :: Text -> Text
surroundQuotes = surround "“" "”"

breakOnMaybe :: Text -> Text -> Maybe (Text, Text)
breakOnMaybe needle haystack =
  case breakOn needle haystack of
    (_, "") -> Nothing
    a       -> Just a

none
  :: Foldable t
  => (a -> Bool) -> t a -> Bool
none f = not . any f
