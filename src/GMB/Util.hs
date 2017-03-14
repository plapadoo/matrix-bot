{-# LANGUAGE OverloadedStrings #-}
module GMB.Util(
    putLog
  , forceEither
  , textShow
  , surround
  , surroundHtml
  , surroundQuotes
  , breakOnMaybe
  , textHashAsText) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Crypto.Hash            (Digest, SHA3_512, hash, hashlazy)
import           Data.ByteString        (ByteString)
import           Data.Either            (Either (..))
import           Data.Eq                ((==))
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.Maybe             (Maybe (..))
import           Data.Monoid            (Monoid, (<>))
import           Data.Text              (Text, pack, unpack,breakOn)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Text.IO           (appendFile, putStr)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Prelude                (error)
import           System.FilePath        (FilePath)
import           Text.Show              (Show, show)

textHashAsText :: Text -> Text
textHashAsText = pack . show . (hash :: ByteString -> Digest SHA3_512) . encodeUtf8

putLog :: MonadIO m => FilePath -> Text -> m ()
putLog logFile str = do
  currentTime <- liftIO (formatISO8601Millis <$> getCurrentTime)
  let text =(pack currentTime <> " " <> str <> "\n")
  if logFile == "-"
    then liftIO (putStr text)
    else liftIO (appendFile logFile text)

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
