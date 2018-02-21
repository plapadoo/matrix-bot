module Web.Matrix.Bot.API(sendMessage) where

import           Data.Bifunctor                 (second)
import           Data.Bool                      (Bool (..))
import           Data.ByteString.Lazy           (ByteString)
import           Data.Eq                        ((==))
import           Data.Function                  (($), (.))
import           Data.Functor                   (Functor, (<$>))
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import           Data.Text.Lazy                 (fromStrict)
import           Data.Text.Lazy.Encoding        (encodeUtf8)
import           Lucid                          (Html)
import           Plpd.Http                      (HttpMethod (..),
                                                 HttpRequest (..),
                                                 HttpResponse (..),
                                                 MonadHttp (..))
import           Web.Matrix.Bot.IncomingMessage (IncomingMessage,
                                                 coparseIncomingMessage)

responseToBool :: HttpResponse ByteString -> Bool
responseToBool (HttpResponse 200 "success") = True
responseToBool _                            = False

sendMessage :: (Functor m, MonadHttp m) => Text -> Text -> IncomingMessage Text (Html ()) -> m Bool
sendMessage url room incomingMessage =
  let request = HttpRequest {
          _hrUrl = url <> "/" <> room
        , _hrMethod = HttpMethodPost
        , _hrContentType = "text/plain"
        , _hrContent = encodeUtf8 . fromStrict . coparseIncomingMessage $ incomingMessage
        }
  in responseToBool <$> httpRequest request
