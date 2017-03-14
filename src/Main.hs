{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM.TChan (newTChanIO, tryReadTChan,
                                               writeTChan)
import           Control.Lens                 (to, (^.))
import           Control.Monad                (forM, forM_, return, void)
import Control.Applicative((*>),(<*))
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
import Data.Text.Lazy(toStrict)
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.Lazy.Encoding           (decodeUtf8)
import           Data.Text.IO                 (putStrLn)
import Data.Bool(otherwise)
import           Web.Scotty             (body,
                                         param, post,
                                         scotty, setHeader, status, text)
import           GMB.ConfigOptions            (coListenPort, coLogFile,
                                               coMatrixBasePath,
                                               coMatrixPassword,
                                               coMatrixUserName,
                                               readConfigOptions)
import           GMB.Gitlab                   (GitlabEvent, commitMessage,
                                               eventCommits,
                                               eventObjectAttributes,
                                               eventObjectKind, eventRepository,
                                               eventUserName, eventUserUserName,
                                               objectNote, objectTitle,
                                               objectState,
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
import Data.Either(Either)
import Data.String(String)
import           System.FilePath              ((</>))
import           System.IO                    (IO)

messageTxnId :: Text.Text -> Text.Text -> Text.Text
messageTxnId accessToken message = textHashAsText (accessToken <> message)

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

data IncomingMessageBody = BodyWithMarkup Text.Text Text.Text
                         | BodyWithoutMakrup Text.Text

parseMessageBody :: Text.Text -> Either String IncomingMessageBody
parseMessageBody mb | "<body>" `Text.isPrefixOf` mb && "</body>" `Text.isSuffixOf` mb = 
  Atto.parseOnly (Atto.string "<body>" *>  <* Atto.string "</body>") mb

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  let logFile = configOptions ^. coLogFile
      context = MatrixContext (configOptions ^. coMatrixBasePath) (configOptions ^. coLogFile)
  putLog logFile $ "Logging in..."
  loginReply <- login context (MatrixLoginRequest (configOptions ^. coMatrixUserName) (configOptions ^. coMatrixPassword))
  case loginReply ^. mlrpError of
    Nothing -> do
      putLog logFile $ "Login successful..."
      let accessToken = loginReply ^. mlrpAccessToken . to fromJust
          listenPort = configOptions ^. coListenPort
      putLog logFile $ "Listening on " <> (textShow listenPort)
      scotty listenPort $ do
        post "/:channel" $ do
          channel <- toStrict <$> param "channel"
          wholeBody <- decodeUtf8 <$> body
          
          text "lol"
    Just e -> error $ "login failed: " <> (Text.unpack e)
