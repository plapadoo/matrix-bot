{-# LANGUAGE TemplateHaskell #-}
module GMB.Http(HttpMethod(..),HttpRequest(..),hrUrl,hrMethod,hrContentType,hrContent,hresStatusCode,hresContent,jsonHttpRequest) where

import           Control.Lens               (makeLenses)
import           Control.Lens               (makeLenses, to, view, (&), (.~),
                                             (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Aeson                 (eitherDecode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
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

jsonHttpRequest :: (ToJSON input,FromJSON output,MonadIO m) => HttpRequest input -> m (HttpResponse output)
jsonHttpRequest request = do
  let opts = defaults & checkStatus .~ Just trivialStatusChecker
  let method = case request ^. hrMethod of HttpMethodPost -> postWith; HttpMethodPut -> putWith
  response <- liftIO $ method opts (Text.unpack (request ^. hrUrl)) (toJSON (request ^. hrContent))
  let responseCode = response ^. responseStatus . statusCode
  case responseCode of
    200 -> do
      case eitherDecode (response ^. responseBody) of
        Left e -> error e
        Right e -> return (HttpResponse (response ^. responseStatus . statusCode) e)
    other -> do
      let bodyText = response ^. responseBody . to BSL8.unpack
      error $ "status code was " <> show other <> ", content was " <> (if null bodyText then "empty" else bodyText)
