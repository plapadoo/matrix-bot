{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Applicative, (<*>))
import Control.Lens (to, view, (^.))
import Control.Monad (Monad, return)
import Data.Either (Either(..))
import Data.String (String)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Eq ((==))
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.IO (appendFile, putStr)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.UUID (UUID, toText)
import GMB.ConfigOptions
       (ConfigOptions, coListenPort, coLogFile, coMatrixBasePath,
        coMatrixPassword, coMatrixUserName, readConfigOptions)
import Plpd.Http
       (MonadHttp(..), hrContent, hrContentType, hrMethod, hrUrl,loggingHttp,
        hresStatusCode, hresContent)
import GMB.Matrix
       (MatrixContext(..), MatrixLoginRequest(..), MonadMatrix(..),
        joinRoomImpl, login, loginImpl, mlrpAccessToken, mlrpError,
        sendMessageImpl)
import Plpd.MonadLog (MonadLog(..),defaultLog)
import GMB.ProgramOptions (poConfigFile, readProgramOptions)
import Plpd.Util (textShow)
import GMB.WebServer (webServer)
import Prelude (error)
import System.IO (IO)
import System.Random (randomIO)

{-
callback :: MatrixContext -> Text.Text -> RepoMapping -> GitlabEvent -> IO ()
callback context accessToken repoMapping event = do
  case eventObjectKind event of
    "push" -> do
      let repo = fromJust (eventRepository event)
          commitCount = maybe "0" textShow (length <$> (eventCommits event))
          userName = fold (eventUserName event)
          commits = fold ((Text.intercalate ", " . ((surroundQuotes . Text.strip . commitMessage) <$>)) <$> eventCommits event)
          message = userName <> " pushed " <> commitCount <> " commit(s) to " <> repositoryName repo <> ": " <> commits
          formattedMessage = (surroundHtml "strong" userName) <> " pushed " <> commitCount <> " commit(s) to " <> surroundHtml "strong" (repositoryName repo) <> ": " <> commits
      forM_ (roomsForEntity repoMapping (RoomToRepo (Repo (repositoryName repo)))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken message) room message formattedMessage)
    "issue" -> do
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          state = fromJust (objectState attributes)
          message = userName <> " " <> state <> " issue " <> (surroundQuotes . fromJust . objectTitle $ attributes) <> " to " <> repositoryName repo
          issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes . fromJust . objectTitle $ attributes) <> "</a>"
          formattedMessage = (surroundHtml "strong" userName) <> " added issue " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
      forM_ (roomsForEntity repoMapping (RoomToRepo (Repo (repositoryName repo)))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken message) room message formattedMessage)
    "note" -> do
      let repo = fromJust (eventRepository event)
          userName = fold (eventUserUserName event)
          attributes = fromJust (eventObjectAttributes event)
          message = userName <> " commented " <> (surroundQuotes . fromJust . objectNote $ attributes) <> " to " <> repositoryName repo
          issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes . fromJust . objectNote $ attributes) <> "</a>"
          formattedMessage = (surroundHtml "strong" userName) <> " commented " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
      forM_ (roomsForEntity repoMapping (RoomToRepo (Repo (repositoryName repo)))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken message) room message formattedMessage)
    unknown ->
      putLog (context ^. mcLogFile) $ "got event without repository, not continuing"

joinRooms logFile accessToken repoMapping context = do
  putLog logFile $ "Login success, access token: " <> accessToken
  forM_ (rooms repoMapping) $ \(Room room) -> do
    joinReply <- joinRoom context (MatrixJoinRequest accessToken room)
    case joinReply ^. mjrpError of
      Nothing -> putLog logFile $ "Join " <> room <> " success"
      Just e  -> error $ "join failed: " <> (Text.unpack e)
  putLog logFile $ "All rooms joined"

applyMonitors logFile accessToken repoMapping context = do
  forM_ (roomToDirs repoMapping) $ \((Room room),(Directory dir)) -> do
    watcher <- watchRecursiveBuffering (Text.unpack dir)
    unbuffer watcher $ \events -> do
      let message (NotifyEvent path event) =
            case event of
              Modified _ fp -> Just ("modified " <> Text.pack (maybe path (\p -> path </> p) fp))
              Attributes _ fp -> Just ("attributes changed " <> Text.pack (maybe path (\p -> path </> p) fp))
              MovedOut _ fp _ -> Just ("moved out " <> Text.pack (path </> fp))
              MovedIn _ fp _ -> Just ("moved in " <> Text.pack (path </> fp))
              MovedSelf fp -> Just ("moved self " <> Text.pack path)
              Created _ fp -> Just ("created " <> Text.pack (path </> fp))
              Deleted _ fp -> Just ("deleted " <> Text.pack (path </> fp))
              _ -> Nothing
      case catMaybes (message <$> events) of
        [] -> return ()
        xs -> do
          let lengthLimit = 20
          let formatMessages xs = Text.intercalate ", " xs
          let wholeMessage =
                if length xs >= lengthLimit
                  then textShow (length xs) <> " change(s), showing first " <> textShow lengthLimit <> ": " <> formatMessages (take lengthLimit xs)
                  else textShow (length xs) <> " change(s): " <> formatMessages xs
          void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken wholeMessage) room wholeMessage) wholeMessage
-}
newtype MyMonad a = MyMonad
    { runMyMonad :: ReaderT ConfigOptions IO a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadReader ConfigOptions)

downToIO :: ConfigOptions -> MyMonad a -> IO a
downToIO co = ((flip runReaderT) co . runMyMonad)

instance MonadLog MyMonad where
    putLog inputText = do
        logFile <- view coLogFile
        defaultLog logFile inputText

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
    result <- runReaderT (runMyMonad (initApplication)) configOptions
    case result of
        Left e -> error e
        Right accessToken ->
            webServer
                (configOptions ^. coListenPort)
                (MatrixContext (configOptions ^. coMatrixBasePath))
                accessToken
                (downToIO configOptions)

initApplication :: MyMonad (Either String Text.Text)
initApplication = do
    context <- MatrixContext <$> (view coMatrixBasePath)
    putLog "Logging in..."
    loginRequest <-
        MatrixLoginRequest <$> (view coMatrixUserName) <*>
        (view coMatrixPassword)
    loginReply <- login context loginRequest
    case loginReply ^. mlrpError of
        Nothing -> do
            putLog "Login successful..."
            return (Right (loginReply ^. mlrpAccessToken . to fromJust))
        Just e -> return (Left ("login failed: " <> (Text.unpack e)))
