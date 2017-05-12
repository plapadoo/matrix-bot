{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Applicative, (<*>))
import Control.Lens (to, view, (^.),makeLenses)
import Control.Monad (Monad, return,void)
import Data.Either (Either(..))
import Data.String (String)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Eq ((==))
import Data.Function (flip, ($), (.),const)
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.IO (appendFile, putStr)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.UUID (UUID, toText)
import Web.Matrix.Bot.ConfigOptions
       (ConfigOptions, coListenPort, coLogFile, coMatrixBasePath,
        coMatrixPassword, coMatrixUserName, readConfigOptions)
import Plpd.Http
       (MonadHttp(..), hrContent, hrContentType, hrMethod, hrUrl,loggingHttp,
        hresStatusCode, hresContent)
import Web.Matrix.API
       (MatrixContext(..), MatrixLoginRequest(..), MonadMatrix(..),
        joinRoomImpl, login, loginImpl, mlrpAccessToken, mlrpError,
        sendMessageImpl)
import Plpd.MonadLog (MonadLog(..),defaultLog)
import Web.Matrix.Bot.ProgramOptions (poConfigFile, readProgramOptions)
import Plpd.Util (textShow)
import Web.Matrix.Bot.WebServer (webServer)
import Prelude (error)
import System.IO (IO)
import System.Random (randomIO)
import Control.Concurrent.MVar(MVar,newMVar,modifyMVar_)

data MyDynamicState = MyDynamicState {
    _dynStateConfigOptions :: ConfigOptions
  , _dynStateLogMVar :: MVar ()
  }

makeLenses ''MyDynamicState

newtype MyMonad a = MyMonad
    { runMyMonad :: ReaderT MyDynamicState IO a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadReader MyDynamicState)

downToIO :: MyDynamicState -> MyMonad a -> IO a
downToIO co = ((flip runReaderT) co . runMyMonad)

instance MonadLog MyMonad where
    putLog inputText = do
        logFile <- view (dynStateConfigOptions . coLogFile)
        mvar <- (view dynStateLogMVar)
        liftIO $ modifyMVar_ mvar $ const (defaultLog logFile inputText)

instance MonadHttp MyMonad where
    httpRequest = loggingHttp

instance MonadMatrix MyMonad where
    login = loginImpl
    joinRoom = joinRoomImpl
    sendMessage = sendMessageImpl

main :: IO ()
main = do
    options <- readProgramOptions
    configOptions <- readConfigOptions (options ^. poConfigFile)
    mvar <- newMVar ()
    let dynState = MyDynamicState configOptions mvar
    result <- runReaderT (runMyMonad (initApplication)) dynState
    case result of
        Left e -> error e
        Right accessToken ->
            webServer
                (configOptions ^. coListenPort)
                (MatrixContext (configOptions ^. coMatrixBasePath))
                accessToken
                (downToIO dynState)

initApplication :: MyMonad (Either String Text.Text)
initApplication = do
    context <- MatrixContext <$> (view (dynStateConfigOptions . coMatrixBasePath ))
    putLog "Logging in..."
    loginRequest <-
        MatrixLoginRequest <$> (view (dynStateConfigOptions . coMatrixUserName )) <*>
        (view (dynStateConfigOptions . coMatrixPassword ))
    loginReply <- login context loginRequest
    case loginReply ^. mlrpError of
        Nothing -> do
            putLog "Login successful..."
            return (Right (loginReply ^. mlrpAccessToken . to fromJust))
        Just e -> return (Left ("login failed: " <> (Text.unpack e)))
