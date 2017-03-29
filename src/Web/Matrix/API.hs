{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.API
  (loginImpl
  ,joinRoomImpl
  ,sendMessageImpl
  ,mlrpErrCode
  ,mlrpError
  ,mlrpAccessToken
  ,mjrpError
  ,messageTxnId
  ,MonadMatrix(..)
  ,MatrixLoginRequest(..)
  ,MatrixLoginReply(..)
  ,MatrixJoinRequest(..)
  ,MatrixJoinReply(..)
  ,MatrixSendMessageRequest(..)
  ,MatrixContext(..)
  ,MatrixSendMessageReply(..))
  where

import System.IO (IO)
import Control.Applicative (Applicative, (<*>))
import Control.Lens (makeLenses, to, view, (&), (.~), (^.))
import Control.Monad (Monad, return)
import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, (.:?), (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.Either (Either(..), either)
import Data.Function (id, ($), (.))
import Data.Functor (Functor(..), (<$>))
import Data.Functor.Sum (Sum)
import Data.Int (Int)
import Data.List (null)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.String (String)
import qualified Data.Text as Text
import Debug.Trace (traceShowId)
import Plpd.Http
       (HttpMethod(..), HttpRequest(..), HttpResponse, MonadHttp,
        hresContent, jsonHttpRequest)
import Plpd.MonadLog (MonadLog(..))
import Plpd.Util (forceEither, textHashAsText)
import Prelude (error, undefined)
import System.FilePath
import Text.Show (show, Show)

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
    { _mlrpErrCode :: Maybe Text.Text
    , _mlrpError :: Maybe Text.Text
    , _mlrpAccessToken :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixLoginReply where
    parseJSON (Object v) =
        MatrixLoginReply <$> (v .:? "errcode") <*> (v .:? "error") <*>
        (v .:? "access_token")

makeLenses ''MatrixLoginReply

data MatrixJoinRequest = MatrixJoinRequest
    { _mjrAccessToken :: Text.Text
    , _mjrRoomId :: Text.Text
    } deriving ((Show))

instance ToJSON MatrixJoinRequest where
    toJSON v = object []

makeLenses ''MatrixJoinRequest

data MatrixJoinReply = MatrixJoinReply
    { _mjrpErrCode :: Maybe Text.Text
    , _mjrpError :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixJoinReply where
    parseJSON (Object v) =
        MatrixJoinReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixJoinReply

data MatrixSendMessageRequest = MatrixSendMessageRequest
    { _msmAccessToken :: Text.Text
    , _msmTxnId :: Text.Text
    , _msmRoomId :: Text.Text
    , _msmMessage :: Text.Text
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
    , _msmrpError :: Maybe Text.Text
    } deriving ((Show))

instance FromJSON MatrixSendMessageReply where
    parseJSON (Object v) =
        MatrixSendMessageReply <$> (v .:? "errcode") <*> (v .:? "error")

makeLenses ''MatrixSendMessageReply

data MatrixContext = MatrixContext
    { _mcBaseUrl :: Text.Text
    }

makeLenses ''MatrixContext

class MonadMatrix m  where
    login :: MatrixContext -> MatrixLoginRequest -> m MatrixLoginReply
    joinRoom :: MatrixContext -> MatrixJoinRequest -> m MatrixJoinReply
    sendMessage :: MatrixContext
                -> MatrixSendMessageRequest
                -> m MatrixSendMessageReply

instance MonadMatrix IO where
    login = loginImpl
    joinRoom = joinRoomImpl
    sendMessage = sendMessageImpl

viewReply :: HttpResponse (Either String c) -> c
viewReply = forceEither . view hresContent

requestThenViewReply
    :: (MonadHttp f, Functor f, ToJSON input, FromJSON output)
    => Text.Text -> input -> HttpMethod -> f output
requestThenViewReply url request method =
    viewReply <$>
    (jsonHttpRequest (HttpRequest url method "application/json" request))

loginImpl
    :: (Functor m, MonadHttp m)
    => MatrixContext -> MatrixLoginRequest -> m MatrixLoginReply
loginImpl context request =
    let url = (context ^. mcBaseUrl) <> "_matrix/client/r0/login"
    in requestThenViewReply url request HttpMethodPost

joinRoomImpl
    :: (Functor m, MonadHttp m)
    => MatrixContext -> MatrixJoinRequest -> m MatrixJoinReply
joinRoomImpl context request =
    let url =
            (context ^. mcBaseUrl) <> "_matrix/client/r0/rooms/" <>
            (request ^. mjrRoomId) <>
            "/join?access_token=" <>
            (request ^. mjrAccessToken)
    in requestThenViewReply url request HttpMethodPost

sendMessageImpl
    :: (Functor m, MonadHttp m)
    => MatrixContext -> MatrixSendMessageRequest -> m MatrixSendMessageReply
sendMessageImpl context request =
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
