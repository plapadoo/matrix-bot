{-# LANGUAGE OverloadedStrings #-}
module GMB.MonadLog where

import           Data.Eq           ((==))
import           Data.Functor      ((<$>))
import           Data.Monoid       ((<>))
import           Data.Text         (Text, breakOn, pack, unpack)
import           Data.Text.IO      (appendFile, putStr)
import           Data.Time.Clock   (getCurrentTime)
import           Data.Time.ISO8601 (formatISO8601Millis)
import           Prelude           ()
import           System.FilePath   (FilePath)
import           System.IO         (IO)

class MonadLog m where
  putLog :: FilePath -> Text -> m ()

instance MonadLog IO where
  putLog logFile str = do
    currentTime <- formatISO8601Millis <$> getCurrentTime
    let text =(pack currentTime <> " " <> str <> "\n")
    if logFile == "-"
      then putStr text
      else appendFile logFile text
