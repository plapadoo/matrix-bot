module Plpd.MonadLog
  ( MonadLog(..)
  , defaultLog
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Eq                ((==))
import           Data.Function          (($))
import           Data.Functor           ((<$>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, breakOn, pack, unpack)
import           Data.Text.IO           (appendFile, hPutStr, putStr)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Prelude                ()
import           System.FilePath        (FilePath)
import           System.IO              (IOMode (AppendMode), hSetEncoding,
                                         stderr, stdout, utf8, withFile)

class MonadLog m where
  putLog :: Text -> m ()

defaultLog :: MonadIO m => FilePath -> Text -> m ()
defaultLog logFile inputText = do
  currentTime <- liftIO (formatISO8601Millis <$> getCurrentTime)
  let t = pack currentTime <> " " <> inputText <> "\n"
  liftIO $
    if logFile == "-"
      then do
        liftIO (hSetEncoding stdout utf8)
        liftIO (hSetEncoding stderr utf8)
        putStr t
      else
        liftIO $ withFile logFile AppendMode $ \h -> do
            hSetEncoding h utf8
            hSetEncoding h utf8
            hPutStr h t
