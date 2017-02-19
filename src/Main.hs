{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens  ((^.),to)
import           Data.Function (($), (.))
import           Data.Maybe    (Maybe (..))
import           Data.Monoid   ((<>))
import qualified Data.Text     as Text
import           GMB.Matrix    (MatrixContext (..), MatrixJoinRequest (..),
                                MatrixLoginRequest (..),
                                MatrixSendMessageRequest (..), joinRoom, login,
                                mlrpAccessToken, mlrpError, mlrpErrCode,
                                sendMessage)
import           Prelude       (error, undefined)
import           System.IO     (IO)
import Data.Maybe(fromJust)
import Data.Text.IO(putStrLn)

main :: IO ()
main = do
  let context = MatrixContext "https://foo/" "/tmp/log.txt"
  loginReply <- login context (MatrixLoginRequest "gitbot" "")
  case loginReply ^. mlrpError of
    Nothing -> putStrLn $ "Login success, access token: " <> (loginReply ^. mlrpAccessToken .to fromJust)
    Just e -> error $ "login failed: " <> (Text.unpack e)
