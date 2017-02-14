{-# LANGUAGE OverloadedStrings #-}
module GMB.Util(putLog) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Eq                ((==))
import           Data.Functor           ((<$>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)
import           Data.Text.IO           (appendFile, putStr)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Prelude                ()
import           System.FilePath        (FilePath)

putLog :: MonadIO m => FilePath -> Text -> m ()
putLog logFile str = do
  currentTime <- liftIO (formatISO8601Millis <$> getCurrentTime)
  let text =(pack currentTime <> " " <> str <> "\n")
  if logFile == "-"
    then liftIO (putStr text)
    else liftIO (appendFile logFile text)
