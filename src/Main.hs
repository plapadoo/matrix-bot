{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens  ((^.),to)
import           Data.Function (($), (.))
import           Data.Maybe    (Maybe (..))
import           Data.Monoid   ((<>))
import Control.Monad(return)
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

main :: IO ()
main = do
  let context = MatrixContext "" "/tmp/log.txt"
  loginReply <- login context (MatrixLoginRequest "gitbot" "")
  case loginReply ^. mlrpError of
    Nothing -> do
      let accessToken = loginReply ^. mlrpAccessToken .to fromJust
      putStrLn $ "Login success, access token: " <> accessToken
      let room = "!EGRmFUjzhLyGZapmhn:chat.plapadoo.de"
      joinReply <- joinRoom context (MatrixJoinRequest accessToken room)
      case joinReply ^. mjrpError of
        Nothing -> do
          putStrLn $ "Join success"
          sendReply <- sendMessage context (MatrixSendMessageRequest accessToken 1 room "Haskell hi!")
          return ()
        Just e -> error $ "join failed: " <> (Text.unpack e)
    Just e -> error $ "login failed: " <> (Text.unpack e)
