{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.ConfigOptions(ConfigOptions(..),readConfigOptions,coLogFile,coMappingsFile,coGitlabListenPort,coMatrixUserName,coMatrixPassword) where

import           Control.Applicative    ((<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Configurator      (Worth (..), load, require)
import           Data.Functor           ((<$>))
import           Data.Int               (Int)
import qualified Data.Text              as Text
import           Prelude                (error, undefined)
import           System.FilePath

data ConfigOptions = ConfigOptions {
    _coLogFile          :: FilePath
  , _coMappingsFile     :: FilePath
  , _coGitlabListenPort :: Int
  , _coMatrixUserName   :: Text.Text
  , _coMatrixPassword   :: Text.Text
  }

makeLenses ''ConfigOptions

requireLift config key = liftIO (require config key)

readConfigOptions :: MonadIO m => FilePath -> m ConfigOptions
readConfigOptions configFilePath = do
  config <- liftIO (load [Required configFilePath])
  ConfigOptions <$> (requireLift config "log-file")
                <*> (requireLift config "mappings-file")
                <*> (requireLift config "gitlab.listen-port")
                <*> (requireLift config "matrix.username")
                <*> (requireLift config "matrix.password")
