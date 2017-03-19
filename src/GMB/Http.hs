{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.Http(
    HttpMethod(..)
  , HttpRequest(..)
  , MonadHttp(..)
  , hrUrl
  , hrMethod
  , hrContentType
  , hrContent
  , hresStatusCode
  , hresContent) where

import GMB.MonadLog(MonadLog(..))
import           Control.Lens               (makeLenses, to, view, (&), (.~),
                                             (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Aeson                 (eitherDecode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           GMB.Util                   (textShow)
import           Network.Wreq               (checkStatus, defaults, header,
                                             linkURL, param, postWith, putWith,
                                             responseBody, responseLink,
                                             responseStatus, statusCode)

-- HTTP API abstraction
data HttpMethod = HttpMethodPost | HttpMethodPut

data HttpRequest a = HttpRequest {
    _hrUrl         :: Text.Text
  , _hrMethod      :: HttpMethod
  , _hrContentType :: Text.Text
  , _hrContent     :: a
  }

makeLenses ''HttpRequest

data HttpResponse a = HttpResponse {
    _hresStatusCode :: Int
  , _hresContent    :: a
  }

trivialStatusChecker :: t2 -> t1 -> t -> Maybe a
trivialStatusChecker _ _ _ = Nothing

makeLenses ''HttpResponse

class MonadHttp m where
  jsonHttpRequest :: (ToJSON input,FromJSON output) => FilePath -> HttpRequest input -> m (HttpResponse output)

instance MonadHttp IO where
  jsonHttpRequest logFile request = do
    let opts = defaults & checkStatus .~ Just trivialStatusChecker
    let method = case request ^. hrMethod of HttpMethodPost -> postWith; HttpMethodPut -> putWith
    let methodString = case request ^. hrMethod of HttpMethodPost -> "POST"; HttpMethodPut -> "PUT"
    putLog logFile $ "Sending HTTP " <> methodString <> " to " <> request ^. hrUrl
    response <- liftIO $ method opts (Text.unpack (request ^. hrUrl)) (toJSON (request ^. hrContent))
    let responseCode = response ^. responseStatus . statusCode
    case responseCode of
      200 -> do
        putLog logFile $ "HTTP result OK"
        case eitherDecode (response ^. responseBody) of
          Left e -> error e
          Right e -> return (HttpResponse (response ^. responseStatus . statusCode) e)
      other -> do
        putLog logFile $ "HTTP result " <> textShow other
        let bodyText = response ^. responseBody . to BSL8.unpack
        error $ "status code was " <> show other <> ", content was " <> (if null bodyText then "empty" else bodyText)
