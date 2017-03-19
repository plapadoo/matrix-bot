{-# LANGUAGE OverloadedStrings #-}
module GMB.WebServer(webServer) where

import           Control.Lens              (to, (^.))
import           Control.Monad             (void)
import           Data.Foldable             (fold)
import           Data.Function             (($), (.))
import           Data.Functor              ((<$>))
import           Data.Maybe                (Maybe (..))
import           Data.Monoid               (mempty, (<>))
import           Data.Text.Lazy            (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding   (decodeUtf8)
import           GMB.IncomingMessage       (markupBody, parseIncomingMessage,
                                            plainBody)
import           GMB.Matrix                (MatrixContext (..),
                                            MatrixJoinRequest (..),
                                            MatrixLoginRequest (..),
                                            MatrixSendMessageRequest (..),
                                            joinRoom, login, mcLogFile,
                                            mjrpError, mlrpAccessToken,
                                            mlrpErrCode, mlrpError, sendMessage,messageTxnId)
import           GMB.Util                  (breakOnMaybe, forceEither, putLog,
                                            surroundHtml, surroundQuotes,
                                            textHashAsText, textShow)
import           Network.HTTP.Types.Status (badRequest400)
import           Prelude                   ()
import           Web.Scotty                (body, param, post, scotty,
                                            setHeader, status, text)

webServer logFile listenPort context accessToken = scotty listenPort $ do
  post "/:room" $ do
    room <- toStrict <$> param "room"
    joinReply <- joinRoom context (MatrixJoinRequest accessToken room)
    case joinReply ^. mjrpError of
      Nothing -> do
        putLog logFile $ "Join " <> room <> " success"
        wholeBody <- (parseIncomingMessage . toStrict . decodeUtf8) <$> body
        text "success"
        let plain = wholeBody ^. plainBody
            markup = fold (wholeBody ^. markupBody)
        void $ sendMessage context (MatrixSendMessageRequest accessToken (messageTxnId accessToken plain) room plain markup)
      Just e  -> do
        putLog logFile $ "join " <> room <> " failed: " <> e
        text $ fromStrict ("join failed: " <> e)
        status badRequest400
