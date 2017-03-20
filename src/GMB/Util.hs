{-# LANGUAGE OverloadedStrings #-}
module GMB.Util(
    forceEither
  , textShow
  , surround
  , none
  , surroundHtml
  , surroundQuotes
  , breakOnMaybe
  , textHashAsText) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Crypto.Hash            (Digest, SHA3_512, hash, hashlazy)
import           Data.Bool              (Bool, not)
import           Data.ByteString        (ByteString)
import           Data.Either            (Either (..))
import           Data.Eq                ((==))
import           Data.Foldable          (Foldable,any)
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.Maybe             (Maybe (..))
import           Data.Monoid            (Monoid, (<>))
import           Data.Text              (Text, breakOn, pack, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Text.IO           (appendFile, putStr)
import           Prelude                (error)
import           System.FilePath        (FilePath)
import           Text.Show              (Show, show)

textHashAsText :: Text -> Text
textHashAsText = pack . show . (hash :: ByteString -> Digest SHA3_512) . encodeUtf8

textShow :: Show a => a -> Text
textShow = pack . show

surround :: Monoid m => m -> m -> m -> m
surround before after inside = before <> inside <> after

surroundHtml :: Text -> Text -> Text
surroundHtml tagName inside = "<" <> tagName <> ">" <> inside <> "</" <> tagName <> ">"

surroundQuotes :: Text -> Text
surroundQuotes = surround "“" "”"

forceEither :: Show e => Either e a -> a
forceEither (Left e)  = error (show e)
forceEither (Right e) = e

breakOnMaybe :: Text -> Text -> Maybe (Text,Text)
breakOnMaybe needle haystack =
  case breakOn needle haystack of
    (_,"") -> Nothing
    a      -> Just a

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not . any f
