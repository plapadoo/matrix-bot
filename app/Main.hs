{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Control.Applicative           (Applicative, pure, (<*>))
import           Control.Concurrent.MVar       (MVar, modifyMVar_, newMVar)
import           Control.Lens                  (makeLenses, to, view, (^.))
import           Control.Monad                 (Monad, return, void)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader, ReaderT,
                                                runReaderT)
import           Data.Either                   (Either (..))
import           Data.Eq                       ((==))
import           Data.Function                 (const, flip, ($), (.))
import           Data.Functor                  (Functor, (<$>))
import           Data.Maybe                    (Maybe (..), fromJust)
import           Data.Monoid                   ((<>))
import           Data.String                   (String)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (appendFile, putStr)
import           Data.Text.Lazy                (toStrict)
import           Data.Text.Lazy.Encoding       (decodeUtf8)
import           Data.UUID                     (UUID, toText)
import           Plpd.Http                     (hrContent, hrContentType,
                                                hrMethod, hrUrl, hresContent,
                                                hresStatusCode, loggingHttp)
import           Plpd.MonadLog                 (LogMode (..), defaultLog)
import           Plpd.Util                     (textShow)
import           Prelude                       (error)
import           System.IO                     (IO)
import           System.Random                 (randomIO)
import           Web.Matrix.API                (MatrixContext (..),
                                                MatrixLoginRequest (..), login,
                                                mlrpAccessToken, mlrpError)
import           Web.Matrix.Bot.ConfigOptions  (ConfigOptions, coListenPort,
                                                coMatrix, coMatrixBasePath,
                                                coMatrixPassword,
                                                coMatrixUserName,
                                                readConfigOptions)
import           Web.Matrix.Bot.ProgramOptions (poConfigFile,
                                                readProgramOptions)
import           Web.Matrix.Bot.WebServer      (webServer)

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  result <- initApplication configOptions
  case result of
    Left e -> error e
    Right accessToken ->
      webServer
        (configOptions ^. coListenPort)
        (MatrixContext (configOptions ^. coMatrix . coMatrixBasePath))
        accessToken

initApplication :: ConfigOptions -> IO (Either String Text.Text)
initApplication co = do
  defaultLog LogStdout "Logging in..."
  let context = MatrixContext (co ^. coMatrix . coMatrixBasePath)
  let loginRequest = MatrixLoginRequest
                     (co ^.  coMatrix . coMatrixUserName)
                     (co ^. coMatrix . coMatrixPassword)
  loginReply <- login context loginRequest
  case loginReply ^. mlrpError of
    Nothing -> do
      defaultLog LogStdout "Login successful..."
      pure (Right (loginReply ^. mlrpAccessToken . to fromJust))
    Just e -> pure (Left ("login failed: " <> (Text.unpack e)))
