{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens       (to, (^.))
import           Control.Monad      (forM_, return, void)
import           Data.Foldable      (fold, foldMap)
import           Data.Function      (id, ($), (.))
import           Data.Functor       ((<$>))
import           Data.List          (length)
import           Data.Maybe         (Maybe (..))
import           Data.Maybe         (fromJust, maybe)
import           Data.Monoid        (mempty, (<>))
import qualified Data.Text          as Text
import           Data.Text.IO       (putStrLn)
import           GMB.ConfigOptions  (coGitlabListenPort, coLogFile,
                                     coMappingsFile, coMatrixBasePath,
                                     coMatrixPassword, coMatrixUserName,
                                     readConfigOptions)
import           GMB.Gitlab         (GitlabEvent, objectNote,objectTitle,objectUrl,commitMessage, eventCommits,
                                     eventObjectAttributes, eventObjectKind,
                                     eventRepository, eventUserName,
                                     repositoryName)
import           GMB.Matrix         (MatrixContext (..), MatrixJoinRequest (..),
                                     MatrixLoginRequest (..),
                                     MatrixSendMessageRequest (..), joinRoom,
                                     login, mcLogFile, mjrpError,
                                     mlrpAccessToken, mlrpErrCode, mlrpError,
                                     sendMessage)
import           GMB.ProgramOptions (poConfigFile, readProgramOptions)
import           GMB.RepoMapping    (Repo (..), RepoMapping, Room (..),
                                     readRepoMapping, rooms, roomsForRepo)
import           GMB.Util           (forceEither, putLog, surroundHtml,
                                     surroundQuotes, textShow)
import           GMB.WebServer      (ServerData (..), webServer)
import           Prelude            (error, undefined)
import           System.IO          (IO)

callback :: MatrixContext -> Text.Text -> RepoMapping -> GitlabEvent -> IO ()
callback context accessToken repoMapping event = do
  putLog (context ^. mcLogFile) $ "repo mapping: " <> textShow repoMapping
  case eventObjectKind event of
    "push" -> do
      let repo = fromJust (eventRepository event)
      let commitCount = maybe "0" textShow (length <$> (eventCommits event))
      let userName = fold (eventUserName event)
      let commits = fold ((Text.intercalate ", " . ((surroundQuotes . Text.strip . commitMessage) <$>)) <$> eventCommits event)
      let message = userName <> " pushed " <> commitCount <> " commit(s) to " <> repositoryName repo <> ": " <> commits
      let formattedMessage = (surroundHtml "strong" userName) <> " pushed " <> commitCount <> " commit(s) to " <> surroundHtml "strong" (repositoryName repo) <> ": " <> commits
      forM_ (roomsForRepo repoMapping (Repo (repositoryName repo))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken 1 room message formattedMessage)
    "issue" -> do
      let repo = fromJust (eventRepository event)
      let userName = fold (eventUserName event)
      let attributes = fromJust (eventObjectAttributes event)
      let message = userName <> " added issue " <> (surroundQuotes (objectTitle attributes)) <> " to " <> repositoryName repo
      let issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes (objectTitle attributes)) <> "</a>"
      let formattedMessage = (surroundHtml "strong" userName) <> " added issue " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
      forM_ (roomsForRepo repoMapping (Repo (repositoryName repo))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken 1 room message formattedMessage)
    "note" -> do
      let repo = fromJust (eventRepository event)
      let userName = fold (eventUserName event)
      let attributes = fromJust (eventObjectAttributes event)
      let message = userName <> " commented " <> (surroundQuotes (objectNote attributes)) <> " to " <> repositoryName repo
      let issueLink = "<a href=\"" <> (objectUrl attributes) <> "\">" <> (surroundQuotes (objectNote attributes)) <> "</a>"
      let formattedMessage = (surroundHtml "strong" userName) <> " commented " <> issueLink <> " to " <> surroundHtml "strong" (repositoryName repo)
      forM_ (roomsForRepo repoMapping (Repo (repositoryName repo))) $ \(Room room) ->
        void $ sendMessage context (MatrixSendMessageRequest accessToken 1 room message formattedMessage)
    unknown ->
      putLog (context ^. mcLogFile) $ "got event without repository, not continuing"

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  repoMapping <- forceEither <$> readRepoMapping (configOptions ^. coMappingsFile)
  putLog (configOptions ^. coLogFile) $ "Logging in..."
  let context = MatrixContext (configOptions ^. coMatrixBasePath) (configOptions ^. coLogFile)
  loginReply <- login context (MatrixLoginRequest (configOptions ^. coMatrixUserName) (configOptions ^. coMatrixPassword))
  case loginReply ^. mlrpError of
    Nothing -> do
      let accessToken = loginReply ^. mlrpAccessToken .to fromJust
      putLog (configOptions ^. coLogFile) $ "Login success, access token: " <> accessToken
      forM_ (rooms repoMapping) $ \(Room room) -> do
        joinReply <- joinRoom context (MatrixJoinRequest accessToken room)
        case joinReply ^. mjrpError of
          Nothing -> putLog (configOptions ^. coLogFile) $ "Join " <> room <> " success"
          Just e  -> error $ "join failed: " <> (Text.unpack e)
      putLog (configOptions ^. coLogFile) $ "All rooms joined"
      let listenPort = configOptions ^. coGitlabListenPort
      putLog (configOptions ^. coLogFile) $ "Listening on " <> (textShow listenPort)
      webServer listenPort (ServerData (callback context accessToken repoMapping))
    Just e -> error $ "login failed: " <> (Text.unpack e)
