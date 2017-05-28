{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Bot.WebServer
  (webServer
  ,handleMessage
  ,WebServerInput(..)
  ,WebServerOutput(..)
  ,wsiContext
  ,wsiAccessToken
  ,wsiRoom
  ,wsiBody)
  where

import Control.Lens (makeLenses, to, (^.))
import Control.Monad (Monad, return, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Network.Wai.Handler.Warp (Settings,setPort,setOnExceptionResponse)
import Network.Wai (responseLBS)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (fold)
import Data.Function (id, ($), (.))
import Data.Default(def)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as TextLazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Plpd.Http (MonadHttp(..))
import Web.Matrix.Bot.IncomingMessage
       (markupBody, parseIncomingMessage, plainBody)
import Web.Matrix.API
       (MatrixContext(..), MatrixJoinRequest(..), MatrixLoginRequest(..),
        MatrixSendMessageRequest(..), MonadMatrix(..), joinRoom,
        joinRoomImpl, login, loginImpl, messageTxnId, mjrpError,
        mlrpAccessToken, mlrpErrCode, mlrpError, sendMessage,
        sendMessageImpl)
import Plpd.MonadLog (MonadLog(..))
import Plpd.Util
       (breakOnMaybe, forceEither, surroundHtml, surroundQuotes,
        textHashAsText, textShow)
import Network.HTTP.Types.Status
       (Status, badRequest400, forbidden403, ok200,status500)
import Network.Wai (Response)
import Network.Wai.Handler.Warp (Port)
import Prelude ()
import System.FilePath (FilePath)
import Text.Show(show)
import System.IO (IO)
import Web.Scotty.Trans
       (ActionT, ScottyT, body, param, post, scottyOptsT, setHeader, status,Options(..),
        text)

instance (Monad m, MonadHttp m) =>
         MonadHttp (ActionT e m) where
    httpRequest input = lift (httpRequest input)

instance (Monad m, MonadMatrix m) =>
         MonadMatrix (ActionT e m) where
    login context input = lift (login context input)
    sendMessage context input = lift (sendMessage context input)
    joinRoom context input = lift (joinRoom context input)

instance (Monad m, MonadLog m) =>
         MonadLog (ActionT e m) where
    putLog text = lift (putLog text)

data WebServerInput = WebServerInput
    { _wsiContext :: MatrixContext
    , _wsiAccessToken :: Text
    , _wsiRoom :: Text
    , _wsiBody :: ByteString
    }

makeLenses ''WebServerInput

data WebServerOutput =
    WebServerOutput Text
                    Status

handleMessage
    :: (Monad m, MonadLog m, MonadMatrix m)
    => WebServerInput -> m WebServerOutput
handleMessage input = do
    joinReply <-
        joinRoom
            (input ^. wsiContext)
            (MatrixJoinRequest (input ^. wsiAccessToken) (input ^. wsiRoom))
    case joinReply ^. mjrpError of
        Nothing -> do
            putLog $ "Join " <> (input ^. wsiRoom) <> " success"
            let wholeBody =
                    (parseIncomingMessage . TextLazy.toStrict . decodeUtf8)
                        (input ^. wsiBody)
                plain = wholeBody ^. plainBody
                markup = wholeBody ^. markupBody
            putLog $
                "Sending message “" <> plain <> "”, markup “" <> fold markup <>
                "”"
            -- FIXME: This has a very subtle bug: the transaction ID doesn't
            -- on the current time, so two exactly identical messages are
            -- filtered by the server. But I was too lazy to add another
            -- type class "MonadTime" or something here.
            void $
                sendMessage
                    (input ^. wsiContext)
                    (MatrixSendMessageRequest
                         (input ^. wsiAccessToken)
                         (messageTxnId (input ^. wsiAccessToken) plain)
                         (input ^. wsiRoom)
                         plain
                         markup)
            return (WebServerOutput "success" ok200)
        Just e -> do
            putLog $ "join " <> (input ^. wsiRoom) <> " failed: " <> e
            return (WebServerOutput ("join failed: " <> e) forbidden403)

webServer
    :: forall m.
       (MonadIO m, Monad m, MonadLog m, MonadMatrix m)
    => Port -> MatrixContext -> Text -> (m Response -> IO Response) -> IO ()
webServer listenPort context accessToken downToIO =
    scottyOptsT
        (def { settings = (setOnExceptionResponse onE (setPort listenPort (settings def))) })
        downToIO
        (post "/:room" handler :: ScottyT TextLazy.Text m ())
  where
    onE e = responseLBS status500 [] ("Something went wrong: " <> pack (show e))
    handler :: ActionT TextLazy.Text m ()
    handler = do
        room <- TextLazy.toStrict <$> param "room"
        b <- body
        (WebServerOutput resultText resultStatus) <-
            handleMessage (WebServerInput context accessToken room b)
        text (TextLazy.fromStrict resultText)
        status resultStatus
