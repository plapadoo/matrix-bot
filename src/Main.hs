{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM.TChan (newTChanIO, tryReadTChan,
                                               writeTChan)
import           Control.Lens                 (to, (^.))
import           Control.Monad                (forM, forM_, return, void)
import           Data.Foldable                (fold, foldMap, for_)
import           Data.Function                (id, ($), (.))
import           Data.Functor                 ((<$>))
import           Data.Int                     (Int)
import           Data.List                    (length, take)
import           Data.Maybe                   (Maybe (..), catMaybes)
import           Data.Maybe                   (fromJust, maybe)
import           Data.Monoid                  (mempty, (<>))
import           Data.Ord                     ((>=))
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.IO                 (putStrLn)
import           GMB.ConfigOptions            (coGitlabListenPort, coLogFile,
                                               coMappingsFile, coMatrixBasePath,
                                               coMatrixPassword,
                                               coMatrixUserName,
                                               readConfigOptions)
import           GMB.Gitlab                   (GitlabEvent, commitMessage,
                                               eventCommits,
                                               eventObjectAttributes,
                                               eventObjectKind, eventRepository,
                                               eventUserName, eventUserUserName,
                                               objectNote, objectTitle,
                                               objectUrl, repositoryName)
import           GMB.INotify                  (Event (..), NotifyEvent (..),
                                               stopWatch, unbuffer,
                                               watchRecursiveBuffering)
import           GMB.Matrix                   (MatrixContext (..),
                                               MatrixJoinRequest (..),
                                               MatrixLoginRequest (..),
                                               MatrixSendMessageRequest (..),
                                               joinRoom, login, mcLogFile,
                                               mjrpError, mlrpAccessToken,
                                               mlrpErrCode, mlrpError,
                                               sendMessage)
import           GMB.ProgramOptions           (poConfigFile, readProgramOptions)
import           GMB.RepoMapping              (Directory (..), Repo (..),
                                               RepoMapping, Room (..),
                                               RoomEntity (..), readRepoMapping,
                                               roomToDirs, rooms,
                                               roomsForEntity)
import           GMB.Util                     (forceEither, putLog,
                                               surroundHtml, surroundQuotes,
                                               textHashAsText, textShow)
import           GMB.WebServer                (ServerData (..), webServer)
import           Prelude                      (error, undefined)
import           System.FilePath              ((</>))
import           System.IO                    (IO)

messageTxnId :: Text.Text -> Text.Text -> Text.Text
messageTxnId accessToken message = textHashAsText (accessToken <> message)

callback :: MatrixContext -> Text.Text -> RepoMapping -> GitlabEvent -> IO ()
callback context accessToken repoMapping event = do
  case eventObjectKind event of
    "push" -> do
      let repo = fromJust (eventRepository event)
      let commitCount = maybe "0" textShow (length <$> (eventCommits event))
      let userName = fold (eventUserName event)
      let commits = fold ((Text.intercalate ", " . ((surroundQuotes . Text.strip . commitMessage) <$>)) <$> eventCommits event)
      let message = userName <> " pushed " <> commitCount <> " commit(s) to " <> repositoryName repo <> ": " <> commits
      let formattedMessage = (surroundHtml "strong" userName) <> " pushed " <> commitCount <> " commit(s) to " <> surroundHtml "strong" (repositoryName repo) <> ": " <> commits
      forM_ (roomsForEntity repoMapping (RoomToRepo (Repo (repositoryName repo)))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken message) room message formattedMessage)
    "issue" -> do
      let repo = fromJust (eventRepository event)
      let userName = fold (eventUserUserName event)
      let attributes = fromJust (eventObjectAttributes event)
      let message = userName <> " added issue " <> (surroundQuotes . fromJust . objectTitle $ attributes) <> " to " <> repositoryName repo
      let issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes . fromJust . objectTitle $ attributes) <> "</a>"
      let formattedMessage = (surroundHtml "strong" userName) <> " added issue " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
      forM_ (roomsForEntity repoMapping (RoomToRepo (Repo (repositoryName repo)))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken message) room message formattedMessage)
    "note" -> do
      let repo = fromJust (eventRepository event)
      let userName = fold (eventUserUserName event)
      let attributes = fromJust (eventObjectAttributes event)
      let message = userName <> " commented " <> (surroundQuotes . fromJust . objectNote $ attributes) <> " to " <> repositoryName repo
      let issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes . fromJust . objectNote $ attributes) <> "</a>"
      let formattedMessage = (surroundHtml "strong" userName) <> " commented " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
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
          void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken wholeMessage) room wholeMessage wholeMessage)

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  let logFile = configOptions ^. coLogFile
  let context = MatrixContext (configOptions ^. coMatrixBasePath) (configOptions ^. coLogFile)
  repoMapping <- forceEither <$> readRepoMapping (configOptions ^. coMappingsFile)
  putLog logFile $ "repo mapping: " <> textShow repoMapping
  putLog logFile $ "Logging in..."
  loginReply <- login context (MatrixLoginRequest (configOptions ^. coMatrixUserName) (configOptions ^. coMatrixPassword))
  case loginReply ^. mlrpError of
    Nothing -> do
      putLog logFile $ "Login successful..."
      let accessToken = loginReply ^. mlrpAccessToken . to fromJust
      joinRooms logFile accessToken repoMapping context
      applyMonitors logFile accessToken repoMapping context
      for_ (configOptions ^. coGitlabListenPort) $ \listenPort -> do
        putLog logFile $ "Listening on " <> (textShow listenPort)
        webServer listenPort (ServerData (callback context accessToken repoMapping))
    Just e -> error $ "login failed: " <> (Text.unpack e)
