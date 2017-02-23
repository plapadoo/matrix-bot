{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens       (to, (^.))
import           Control.Monad      (forM_, return)
import           Data.Function      (($), (.))
import           Data.Functor       ((<$>))
import           Data.Maybe         (Maybe (..))
import           Data.Maybe         (fromJust)
import           Data.Monoid        ((<>))
import qualified Data.Text          as Text
import           Data.Text.IO       (putStrLn)
import           GMB.ConfigOptions  (coGitlabListenPort, coLogFile,
                                     coMappingsFile, coMatrixBasePath,
                                     coMatrixPassword, coMatrixUserName,
                                     readConfigOptions)
import           GMB.Gitlab         (GitlabEvent, eventRepository,
                                     repositoryName)
import           GMB.Matrix         (MatrixContext (..), MatrixJoinRequest (..),
                                     MatrixLoginRequest (..),
                                     MatrixSendMessageRequest (..), joinRoom,
                                     mcLogFile,
                                     login, mjrpError, mlrpAccessToken,
                                     mlrpErrCode, mlrpError, sendMessage)
import           GMB.ProgramOptions (poConfigFile, readProgramOptions)
import           GMB.RepoMapping    (Repo (..), RepoMapping, Room (..),
                                     readRepoMapping, rooms, roomsForRepo)
import           GMB.Util           (forceEither, textShow,putLog)
import           GMB.WebServer      (ServerData (..), webServer)
import           Prelude            (error, undefined)
import           System.IO          (IO)

callback :: MatrixContext -> Text.Text -> RepoMapping -> GitlabEvent -> IO ()
callback context accessToken repoMapping event = do
  putLog (context ^. mcLogFile) $ "repo mapping: " <> textShow repoMapping
  case (eventRepository event) of
    Nothing -> return ()
    Just repo ->
      forM_ (roomsForRepo repoMapping (Repo (repositoryName repo))) $ \(Room room) -> do
        sendReply <- sendMessage context (MatrixSendMessageRequest accessToken 1 room "Haskell hi!")
        return ()

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
