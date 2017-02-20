{-# LANGUAGE OverloadedStrings #-}
module GMB.Util(putLog,forceEither,textShow) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Eq                ((==))
import           Data.Functor           ((<$>))
import Data.Function((.))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)
import           Data.Text.IO           (appendFile, putStr)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Prelude                (error)
import           System.FilePath        (FilePath)
import Text.Show(Show,show)
import Data.Either(Either(..))

putLog :: MonadIO m => FilePath -> Text -> m ()
putLog logFile str = do
  currentTime <- liftIO (formatISO8601Millis <$> getCurrentTime)
  let text =(pack currentTime <> " " <> str <> "\n")
  if logFile == "-"
    then liftIO (putStr text)
    else liftIO (appendFile logFile text)

textShow :: Show a => a -> Text
textShow = pack . show

forceEither :: Show e => Either e a -> a
forceEither (Left e) = error (show e)
forceEither (Right e) = e
