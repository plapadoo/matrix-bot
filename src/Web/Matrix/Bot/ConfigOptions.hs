{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Bot.ConfigOptions
  (ConfigOptions(..)
  ,readConfigOptions
  ,coLogFile
  ,coListenPort
  ,coListenHost
  ,coMatrixUserName
  ,coMatrixPassword
  ,coMatrixBasePath)
  where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Configurator (Worth(..), load, lookup, require)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe)
import qualified Data.Text as Text
import Prelude (error, undefined)
import System.FilePath

data ConfigOptions = ConfigOptions
    { _coLogFile :: FilePath
    , _coListenPort :: Int
    , _coListenHost :: Maybe Text.Text
    , _coMatrixBasePath :: Text.Text
    , _coMatrixUserName :: Text.Text
    , _coMatrixPassword :: Text.Text
    }

makeLenses ''ConfigOptions

requireLift config key = liftIO (require config key)

lookupLift config key = liftIO (lookup config key)

readConfigOptions
    :: MonadIO m
    => FilePath -> m ConfigOptions
readConfigOptions configFilePath = do
    config <- liftIO (load [Required configFilePath])
    ConfigOptions <$> (requireLift config "log-file") <*>
        (requireLift config "listen-port") <*>
        (lookupLift config "listen-host") <*>
        (requireLift config "matrix.base-path") <*>
        (requireLift config "matrix.username") <*>
        (requireLift config "matrix.password")
