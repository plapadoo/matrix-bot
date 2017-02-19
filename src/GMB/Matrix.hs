{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.Matrix(
  login,
  joinRoom,
  sendMessage,
  mlrpErrCode,
  mlrpError,
  mlrpAccessToken,
  mjrpError,
  MatrixLoginRequest(..),
  MatrixLoginReply,
  MatrixJoinRequest(..),
  MatrixJoinReply,
  MatrixSendMessageRequest(..),
  MatrixContext(..),
  MatrixSendMessageReply) where

import Debug.Trace(traceShowId)
import GMB.Http(HttpMethod(..),hresContent,jsonHttpRequest,HttpRequest(..))
import           Control.Applicative    ((<*>))
import           Control.Lens           (makeLenses, to, view, (&), (.~), (^.))
import           Control.Monad          (return)
import           Control.Monad.Free     (Free, liftF)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         object, (.:?), (.=))
import qualified Data.ByteString.Lazy   as BSL
import           Data.Either            (Either (..))
import           Data.Function          (($), (.))
import           Data.Function          (id)
import           Data.Functor           (Functor (..))
import           Data.Functor           ((<$>))
import           Data.Functor.Sum       (Sum)
import           Data.Int               (Int)
import           Data.List              (null)
import           Data.Maybe             (Maybe (..))
import           Data.Maybe             (Maybe)
import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import           GMB.Util               (putLog)
import           Prelude                (error, undefined)
import           System.FilePath
import           Text.Show              (show)

data MatrixLoginRequest = MatrixLoginRequest {
    _mlrUsername :: Text.Text
  , _mlrPassword :: Text.Text
  }

instance ToJSON MatrixLoginRequest where
  toJSON (MatrixLoginRequest username password) =
    object [
      ( "type" :: Text.Text ) .= ( "m.login.password" :: Text.Text),
      "user" .= username,
      "password" .= password
      ]

makeLenses ''MatrixLoginRequest

data MatrixLoginReply = MatrixLoginReply {
    _mlrpErrCode     :: Maybe Text.Text
  , _mlrpError       :: Maybe Text.Text
  , _mlrpAccessToken :: Maybe Text.Text
  }

instance FromJSON MatrixLoginReply where
  parseJSON (Object v) = MatrixLoginReply <$> (v .:? "errcode") <*> (v .:? "error") <*> (v .:? "access_token")

makeLenses ''MatrixLoginReply

data MatrixJoinRequest = MatrixJoinRequest {
    _mjrAccessToken :: Text.Text
  , _mjrRoomId      :: Text.Text
  }

instance ToJSON MatrixJoinRequest where
  toJSON v = object []

makeLenses ''MatrixJoinRequest

data MatrixJoinReply = MatrixJoinReply {
    _mjrpErrCode :: Maybe Text.Text
  , _mjrpError   :: Maybe Text.Text
  }

instance FromJSON MatrixJoinReply where
  parseJSON (Object v) = MatrixJoinReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixJoinReply

data MatrixSendMessageRequest = MatrixSendMessageRequest {
    _msmAccessToken :: Text.Text
  , _msmTxnId       :: Int
  , _msmRoomId      :: Text.Text
  , _msmMessage     :: Text.Text
  }

instance ToJSON MatrixSendMessageRequest where
  toJSON (MatrixSendMessageRequest _ _ _ message) =
    let msgtype = "m.text" :: Text.Text
    in
      object [
        "msgtype" .= msgtype,
        "body" .= message
        ]

makeLenses ''MatrixSendMessageRequest

data MatrixSendMessageReply = MatrixSendMessageReply {
    _msmrpErrCode :: Maybe Text.Text
  , _msmrpError   :: Maybe Text.Text
  }

instance FromJSON MatrixSendMessageReply where
  parseJSON (Object v) = MatrixSendMessageReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixSendMessageReply

data MatrixContext = MatrixContext {
    _mcBaseUrl :: Text.Text
  , _mcLogFile :: FilePath
  }

makeLenses ''MatrixContext

login :: MonadIO m => MatrixContext -> MatrixLoginRequest -> m MatrixLoginReply
login context request =
  let url = (context ^. mcBaseUrl) <> "_matrix/client/r0/login"
  in view hresContent <$> (jsonHttpRequest (HttpRequest url HttpMethodPost "application/json" request))

joinRoom :: MonadIO m => MatrixContext -> MatrixJoinRequest -> m MatrixJoinReply
joinRoom context request =
  let url = (context ^. mcBaseUrl) <> "_matrix/client/r0/rooms/" <> (request ^. mjrRoomId) <> "/join?access_token=" <> (request ^. mjrAccessToken)
  in view hresContent <$> (jsonHttpRequest (HttpRequest url HttpMethodPost "application/json" request))

sendMessage :: MonadIO m => MatrixContext -> MatrixSendMessageRequest -> m MatrixSendMessageReply
sendMessage context request =
  let url = ((context ^. mcBaseUrl) <> "_matrix/client/r0/rooms/" <> (request ^. msmRoomId) <> "/send/m.room.message/" <> (request ^. msmTxnId . to (Text.pack . show)) <> "?access_token=" <> (request ^. msmAccessToken))
  in view hresContent <$> (jsonHttpRequest (HttpRequest url HttpMethodPut "application/json" request))
