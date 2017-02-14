{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.Matrix(
  login,
  joinRoom,
  sendMessage,
  MatrixLoginRequest,
  MatrixLoginReply,
  MatrixJoinRequest,
  MatrixJoinReply,
  MatrixSendMessageRequest,
  MatrixSendMessageReply) where

import           Control.Applicative    ((<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         object, (.:?), (.=))
import           Data.Functor           ((<$>))
import           Data.Int               (Int)
import           Data.Maybe             (Maybe)
import           Data.Text              (Text,pack)
import           GMB.Util               (putLog)
import           Prelude                (undefined)
import           System.FilePath

data MatrixLoginRequest = MatrixLoginRequest {
    _mlrUsername :: Text
  , _mlrPassword :: Text
  }

instance ToJSON MatrixLoginRequest where
  toJSON (MatrixLoginRequest username password) =
    object [
      ( "type" :: Text ) .= ( "m.login.password" :: Text),
      "user" .= username,
      "password" .= password
      ]

makeLenses ''MatrixLoginRequest

data MatrixLoginReply = MatrixLoginReply {
    _mlrpErrCode     :: Maybe Text
  , _mlrpError       :: Maybe Text
  , _mlrpAccessToken :: Maybe Text
  }

instance FromJSON MatrixLoginReply where
  parseJSON (Object v) = MatrixLoginReply <$> (v .:? "errcode") <*> (v .:? "error") <*> (v .:? "access_token")

makeLenses ''MatrixLoginReply

data MatrixJoinRequest = MatrixJoinRequest {
    _mjrAccessToken :: Text
  , _mjrRoomId      :: Text
  }

instance ToJSON MatrixJoinRequest where
  toJSON v = object []

makeLenses ''MatrixJoinRequest

data MatrixJoinReply = MatrixJoinReply {
    _mjrpErrCode :: Maybe Text
  , _mjrpError   :: Maybe Text
  }

instance FromJSON MatrixJoinReply where
  parseJSON (Object v) = MatrixJoinReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixJoinReply

data MatrixSendMessageRequest = MatrixSendMessageRequest {
    _msmAccessToken :: Text
  , _msmTxnId       :: Int
  , _msmRoomId      :: Text
  , _msmMessage     :: Text
  }

instance ToJSON MatrixSendMessageRequest where
  toJSON (MatrixSendMessageRequest _ _ _ message) =
    object [
      ( "type" :: Text ) .= ( "m.text" :: Text),
      "body" .= message
      ]

makeLenses ''MatrixSendMessageRequest

data MatrixSendMessageReply = MatrixSendMessageReply {
    _msmrpErrCode :: Maybe Text
  , _msmrpError   :: Maybe Text
  }

instance FromJSON MatrixSendMessageReply where
  parseJSON (Object v) = MatrixSendMessageReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixSendMessageReply

data MatrixContext = MatrixContext {
    _mcBaseUrl :: Text
  , _mcLogFile :: FilePath
  }

makeLenses ''MatrixContext

login :: MonadIO m => MatrixContext -> MatrixLoginRequest -> m MatrixLoginReply
login context = do
  let opts = defaults & checkStatus .~ Just trivialStatusChecker
  putLog (logFile ^. mcLogFile) $ " Issuing request on " <> (pack baseUrl)
  response <- liftIO $ getWith opts baseUrl
  case response ^. responseStatus . statusCode of
    200 -> do
      liftIO $ putLog logFile $ "Request was OK 200"
      liftIO $ putLog logFile $ "Next request was " <> showText ( Text.pack <$> (response ^? responseLink "rel" "next" . linkURL . to BS8.unpack) )
      let nextRequest = GitlabIssueRequest <$> (response ^? responseLink "rel" "next" . linkURL . to BS8.unpack)
      bimapEitherT Text.pack (GitlabIssueResult nextRequest . IssueList) (hoistEither (eitherDecode (response ^. responseBody)))
    other -> do
      liftIO $ putLog logFile $ "Request was " <> showText other
      let bodyText = response ^. responseBody . to (decodeUtf8 . toStrict)
      left $ "status code was " <> showText other <> ", content was " <> (if Text.null bodyText then "empty" else bodyText)

joinRoom :: MonadIO m => MatrixJoinRequest -> m MatrixJoinReply
joinRoom = undefined

sendMessage :: MonadIO m => MatrixSendMessageRequest -> m MatrixSendMessageReply
sendMessage = undefined
