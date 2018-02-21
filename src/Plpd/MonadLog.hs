module Plpd.MonadLog
  ( defaultLog
  , LogMode(..)
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Function          (($))
import           Data.Functor           ((<$>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack)
import           Data.Text.IO           (hPutStr)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.ISO8601      (formatISO8601Millis)
import           Prelude                ()
import           System.IO              (Handle, hSetEncoding, stderr, stdout,
                                         utf8)

data LogMode = LogStdout | LogStderr

logModeToHandle :: LogMode -> Handle
logModeToHandle LogStdout = stdout
logModeToHandle LogStderr = stderr

defaultLog :: MonadIO m => LogMode -> Text -> m ()
defaultLog logMode inputText = do
  currentTime <- liftIO (formatISO8601Millis <$> getCurrentTime)
  let t = pack currentTime <> " " <> inputText <> "\n"
  liftIO $ do
    let h = logModeToHandle logMode
    hSetEncoding h utf8
    hPutStr h t
