{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import           Control.Applicative            (pure)
import           Control.Lens                   (makeLenses, (^.))
import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO)
import           Data.ByteString.Lazy           (ByteString)
import           Data.ByteString.Lazy.Char8     (pack)
import           Data.Default                   (def)
import           Data.Foldable                  (fold)
import           Data.Function                  (($), (.))
import           Data.Functor                   ((<$>))
import           Data.Maybe                     (Maybe (..))
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text.Lazy                 as TextLazy
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Network.HTTP.Types.Status      (Status, forbidden403, ok200,
                                                 status500)
import           Network.Wai                    (responseLBS)
import           Network.Wai.Handler.Warp       (setOnExceptionResponse,
                                                 setPort)
import           Network.Wai.Handler.Warp       (Port)
import           Plpd.MonadLog                  (LogMode (..), defaultLog)
import           Prelude                        ()
import           System.IO                      (IO)
import           Text.Show                      (show)
import           Web.Matrix.API                 (MatrixContext (..),
                                                 MatrixJoinRequest (..),
                                                 MatrixSendMessageRequest (..),
                                                 joinRoom, messageTxnId,
                                                 mjrpError, sendMessage)
import           Web.Matrix.Bot.IncomingMessage (markupBody,
                                                 parseIncomingMessage,
                                                 plainBody)
import           Web.Scotty                     (Options (..), body, param,
                                                 post, scottyOpts, status, text)

data WebServerInput = WebServerInput
    { _wsiContext     :: MatrixContext
    , _wsiAccessToken :: Text
    , _wsiRoom        :: Text
    , _wsiBody        :: ByteString
    }

makeLenses ''WebServerInput

data WebServerOutput =
    WebServerOutput Text
                    Status

handleMessage :: MonadIO m => WebServerInput -> m WebServerOutput
handleMessage input = do
  joinReply <-
    joinRoom
      (input ^. wsiContext)
      (MatrixJoinRequest (input ^. wsiAccessToken) (input ^. wsiRoom))
  case joinReply ^. mjrpError of
    Nothing -> do
      defaultLog LogStdout $ "Join " <> (input ^. wsiRoom) <> " success"
      let wholeBody =
            (parseIncomingMessage . TextLazy.toStrict . decodeUtf8)
            (input ^. wsiBody)
          plain = wholeBody ^. plainBody
          markup = wholeBody ^. markupBody
      defaultLog LogStdout $
        "Sending message “" <> plain <> "”, markup “" <> fold markup <> "”"
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
      pure (WebServerOutput "success" ok200)
    Just e -> do
      defaultLog LogStdout $ "join " <> (input ^. wsiRoom) <> " failed: " <> e
      pure (WebServerOutput ("join failed: " <> e) forbidden403)

webServer :: Port -> MatrixContext -> Text -> IO ()
webServer listenPort context accessToken =
  let
    opts = def { settings = setOnExceptionResponse onE (setPort listenPort (settings def)) }
    onE e = responseLBS status500 [] ("Something went wrong: " <> pack (show e))
  in
    scottyOpts opts $
      post "/:room" $ do
        room <- TextLazy.toStrict <$> param "room"
        b <- body
        (WebServerOutput resultText resultStatus) <-
          handleMessage (WebServerInput context accessToken room b)
        text (TextLazy.fromStrict resultText)
        status resultStatus
