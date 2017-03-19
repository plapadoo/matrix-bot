{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.WebServer(
    webServer
  , handleMessage
  , WebServerInput(..)
  , wsiLogFile
  , wsiContext
  , wsiAccessToken
  , wsiRoom
  , wsiBody
  ) where

import           Control.Lens              (makeLenses, to, (^.))
import           Control.Monad             (Monad, return, void)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Foldable             (fold)
import           Data.Function             (id, ($), (.))
import           Data.Functor              ((<$>))
import           Data.Int                  (Int)
import           Data.Maybe                (Maybe (..))
import           Data.Monoid               (mempty, (<>))
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TextLazy
import           Data.Text.Lazy.Encoding   (decodeUtf8)
import           GMB.Http                  (MonadHttp (..))
import           GMB.IncomingMessage       (markupBody, parseIncomingMessage,
                                            plainBody)
import           GMB.Matrix                (MatrixContext (..),
                                            MatrixJoinRequest (..),
                                            MatrixLoginRequest (..),
                                            MatrixSendMessageRequest (..),
                                            MonadMatrix (..), joinRoom, login,
                                            mcLogFile, messageTxnId, mjrpError,
                                            mlrpAccessToken, mlrpErrCode,
                                            mlrpError, sendMessage)
import           GMB.MonadLog              (MonadLog (..))
import           GMB.Util                  (breakOnMaybe, forceEither,
                                            surroundHtml, surroundQuotes,
                                            textHashAsText, textShow)
import           Network.HTTP.Types.Status (Status, badRequest400, ok200)
import           Prelude                   ()
import           System.FilePath           (FilePath)
import           System.IO                 (IO)
import           Web.Scotty                (body, param, post, scotty,
                                            setHeader, status, text)
import           Web.Scotty.Trans          (ActionT)

instance (Monad m,MonadHttp m) => MonadHttp (ActionT e m)
  where jsonHttpRequest fp input = lift (jsonHttpRequest fp input)

instance (Monad m,MonadLog m) => MonadLog (ActionT e m)
  where putLog file text = lift (putLog file text)

data WebServerInput = WebServerInput {
    _wsiLogFile     :: FilePath
  , _wsiContext     :: MatrixContext
  , _wsiAccessToken :: Text
  , _wsiRoom        :: Text
  , _wsiBody        :: ByteString
  }

makeLenses ''WebServerInput

data WebServerOutput = WebServerOutput Text Status

handleMessage :: (MonadIO m,Monad m,MonadLog m,MonadHttp m,MonadMatrix m) => WebServerInput -> m WebServerOutput
handleMessage input = do
    joinReply <- joinRoom (input ^. wsiContext) (MatrixJoinRequest (input ^. wsiAccessToken) (input ^. wsiRoom))
    case joinReply ^. mjrpError of
      Nothing -> do
        putLog (input ^. wsiLogFile) $ "Join " <> (input ^. wsiRoom) <> " success"
        let wholeBody = (parseIncomingMessage . TextLazy.toStrict . decodeUtf8) (input ^. wsiBody)
            plain = wholeBody ^. plainBody
            markup = fold (wholeBody ^. markupBody)
        void $ sendMessage (input ^. wsiContext) (MatrixSendMessageRequest (input ^. wsiAccessToken) (messageTxnId (input ^. wsiAccessToken) plain) (input ^. wsiRoom) plain markup)
        return (WebServerOutput "success" ok200)
      Just e  -> do
        putLog (input ^. wsiLogFile) $ "join " <> (input ^. wsiRoom) <> " failed: " <> e
        return (WebServerOutput ("join failed: " <> e) badRequest400)

webServer :: FilePath -> Int -> MatrixContext -> Text -> IO ()
webServer logFile listenPort context accessToken =
  scotty listenPort $
    post "/:room" $ do
      room <- TextLazy.toStrict <$> param "room"
      b <- body
      (WebServerOutput resultText resultStatus) <- handleMessage (WebServerInput logFile context accessToken room b)
      text (TextLazy.fromStrict resultText)
      status (resultStatus)
