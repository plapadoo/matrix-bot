{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Matrix.API
  ( login
  , joinRoom
  , sendMessage
  , mlrpErrCode
  , mlrpError
  , mlrpAccessToken
  , mjrpError
  , messageTxnId
  , mlrUsername
  , mlrPassword
  , mjrpErrCode
  , msmFormattedMessage
  , msmMessage
  , msmrpErrCode
  , msmrpError
  , MatrixLoginRequest(..)
  , MatrixLoginReply(..)
  , MatrixJoinRequest(..)
  , MatrixJoinReply(..)
  , MatrixSendMessageRequest(..)
  , MatrixContext(..)
  , MatrixSendMessageReply(..))
  where

import           Control.Applicative    ((<*>))
import           Control.Lens           (makeLenses, view, (^.))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         object, (.:?), (.=))
import           Data.Either            (Either (..))
import           Data.Function          ((.))
import           Data.Functor           ((<$>))
import           Data.Maybe             (Maybe (..))
import           Data.Monoid            ((<>))
import           Data.String            (String)
import qualified Data.Text              as Text
import           Plpd.Http              (HttpMethod (..), HttpRequest (..),
                                         HttpResponse, hresContent,
                                         jsonHttpRequest)
import           Plpd.Util              (forceEither, textHashAsText)
import           Prelude                (error)
import           Text.Show              (Show)

data MatrixLoginRequest = MatrixLoginRequest
    { _mlrUsername :: Text.Text
    , _mlrPassword :: Text.Text
    } deriving ((Show))

instance ToJSON MatrixLoginRequest where
    toJSON (MatrixLoginRequest username password) =
        object
            [ ("type" :: Text.Text) .= ("m.login.password" :: Text.Text)
            , "user" .= username
            , "password" .= password]

makeLenses ''MatrixLoginRequest

data MatrixLoginReply = MatrixLoginReply
    { _mlrpErrCode     :: Maybe Text.Text
    , _mlrpError       :: Maybe Text.Text
    , _mlrpAccessToken :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixLoginReply where
    parseJSON (Object v) =
        MatrixLoginReply <$> (v .:? "errcode") <*> (v .:? "error") <*>
        (v .:? "access_token")
    parseJSON _ = error "invalid JSON from login"

makeLenses ''MatrixLoginReply

data MatrixJoinRequest = MatrixJoinRequest
    { _mjrAccessToken :: Text.Text
    , _mjrRoomId      :: Text.Text
    } deriving ((Show))

instance ToJSON MatrixJoinRequest where
    toJSON _ = object []

makeLenses ''MatrixJoinRequest

data MatrixJoinReply = MatrixJoinReply
    { _mjrpErrCode :: Maybe Text.Text
    , _mjrpError   :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixJoinReply where
    parseJSON (Object v) =
        MatrixJoinReply <$> (v .:? "errcode") <*> (v .:? "error")
    parseJSON _ = error "invalid JSON from join"

makeLenses ''MatrixJoinReply

data MatrixSendMessageRequest = MatrixSendMessageRequest
    { _msmAccessToken      :: Text.Text
    , _msmTxnId            :: Text.Text
    , _msmRoomId           :: Text.Text
    , _msmMessage          :: Text.Text
    , _msmFormattedMessage :: Maybe Text.Text
    } deriving ((Show))

instance ToJSON MatrixSendMessageRequest where
    toJSON (MatrixSendMessageRequest _ _ _ message formatted_message) =
        case formatted_message of
            Just markup ->
                let msgtype = "m.notice" :: Text.Text
                    format = "org.matrix.custom.html" :: Text.Text
                in object
                       [ "msgtype" .= msgtype
                       , "format" .= format
                       , "body" .= message
                       , "formatted_body" .= markup]
            Nothing ->
                let msgtype = "m.notice" :: Text.Text
                in object ["msgtype" .= msgtype, "body" .= message]

makeLenses ''MatrixSendMessageRequest

data MatrixSendMessageReply = MatrixSendMessageReply
    { _msmrpErrCode :: Maybe Text.Text
    , _msmrpError   :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixSendMessageReply where
    parseJSON (Object v) =
        MatrixSendMessageReply <$> (v .:? "errcode") <*> (v .:? "error")
    parseJSON _ = error "invalid JSON from message"

makeLenses ''MatrixSendMessageReply

data MatrixContext = MatrixContext
    { _mcBaseUrl :: Text.Text
    }

makeLenses ''MatrixContext

viewReply :: HttpResponse (Either String c) -> c
viewReply = forceEither . view hresContent

requestThenViewReply
    :: (ToJSON input, FromJSON output, MonadIO m)
    => Text.Text -> input -> HttpMethod -> m output
requestThenViewReply url request method =
    viewReply <$>
    jsonHttpRequest (HttpRequest url method "application/json" request)

login :: MonadIO m => MatrixContext -> MatrixLoginRequest -> m MatrixLoginReply
login context request =
    let url = (context ^. mcBaseUrl) <> "_matrix/client/r0/login"
    in requestThenViewReply url request HttpMethodPost

joinRoom :: MonadIO m => MatrixContext -> MatrixJoinRequest -> m MatrixJoinReply
joinRoom context request =
    let url =
            (context ^. mcBaseUrl) <> "_matrix/client/r0/rooms/" <>
            (request ^. mjrRoomId) <>
            "/join?access_token=" <>
            (request ^. mjrAccessToken)
    in requestThenViewReply url request HttpMethodPost

sendMessage :: MonadIO m => MatrixContext -> MatrixSendMessageRequest -> m MatrixSendMessageReply
sendMessage context request =
    let url =
            ((context ^. mcBaseUrl) <> "_matrix/client/r0/rooms/" <>
             (request ^. msmRoomId) <>
             "/send/m.room.message/" <>
             (request ^. msmTxnId) <>
             "?access_token=" <>
             (request ^. msmAccessToken))
    in requestThenViewReply url request HttpMethodPut

messageTxnId :: Text.Text -> Text.Text -> Text.Text
messageTxnId accessToken message = textHashAsText (accessToken <> message)
