{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens  ((^.),to)
import           Data.Function (($), (.))
import           Data.Maybe    (Maybe (..))
import           Data.Monoid   ((<>))
import Control.Monad(return,forM_)
import qualified Data.Text     as Text
import           GMB.Matrix    (MatrixContext (..), MatrixJoinRequest (..),
                                MatrixLoginRequest (..),
                                MatrixSendMessageRequest (..), joinRoom, login,
                                mlrpAccessToken, mlrpError, mlrpErrCode,
                                mjrpError,
                                sendMessage)
import           Prelude       (error, undefined)
import           System.IO     (IO)
import Data.Maybe(fromJust)
import Data.Text.IO(putStrLn)
import GMB.ProgramOptions(readProgramOptions,poConfigFile)
import GMB.ConfigOptions(readConfigOptions,coMappingsFile,coMatrixBasePath,coMatrixUserName,coMatrixPassword,coLogFile)
import GMB.RepoMapping(readRepoMapping)
import           Data.Map               (keys)
import GMB.Util(forceEither)
import Data.Functor((<$>))


main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  repoMapping <- forceEither <$> readRepoMapping (configOptions ^. coMappingsFile)
  let context = MatrixContext (configOptions ^. coMatrixBasePath) (configOptions ^. coLogFile)
  loginReply <- login context (MatrixLoginRequest (configOptions ^. coMatrixUserName) (configOptions ^. coMatrixPassword))
  case loginReply ^. mlrpError of
    Nothing -> do
      let accessToken = loginReply ^. mlrpAccessToken .to fromJust
      putStrLn $ "Login success, access token: " <> accessToken
      forM_ (keys repoMapping) $ \room -> do
        joinReply <- joinRoom context (MatrixJoinRequest accessToken room)
        case joinReply ^. mjrpError of
          Nothing -> do
            putStrLn $ "Join " <> room <> " success"
            sendReply <- sendMessage context (MatrixSendMessageRequest accessToken 1 room "Haskell hi!")
            return ()
          Just e -> error $ "join failed: " <> (Text.unpack e)
    Just e -> error $ "login failed: " <> (Text.unpack e)
