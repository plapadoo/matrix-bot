{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMB.Http
  ( HttpMethod(..)
  , HttpRequest(..)
  , HttpResponse(..)
  , MonadHttp(..)
  , jsonHttpRequest
  , hrUrl
  , hrMethod
  , hrContentType
  , hrContent
  , hresStatusCode
  , hresContent
  ) where

import Control.Lens (makeLenses, to, view, (&), (.~), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
       (FromJSON(..), ToJSON(..), eitherDecode, encode, toJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function (const)
import Data.Functor (Functor)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import GMB.MonadLog (MonadLog(..))
import GMB.Util (textShow)
import Network.Wreq
       (checkResponse, defaults, header, linkURL, param, postWith,
        putWith, responseBody, responseLink, responseStatus, statusCode)
import Network.Wreq.Types (ResponseChecker)

-- HTTP API abstraction
data HttpMethod
  = HttpMethodPost
  | HttpMethodPut

instance Show HttpMethod where
  show HttpMethodPost = "POST"
  show HttpMethodPut = "PUT"

data HttpRequest a = HttpRequest
  { _hrUrl :: Text.Text
  , _hrMethod :: HttpMethod
  , _hrContentType :: Text.Text
  , _hrContent :: a
  } deriving (Functor)

makeLenses ''HttpRequest

data HttpResponse a = HttpResponse
  { _hresStatusCode :: Int
  , _hresContent :: a
  } deriving (Functor)

trivialStatusChecker :: ResponseChecker
trivialStatusChecker _ _ = return ()

makeLenses ''HttpResponse

class MonadHttp m where
  httpRequest :: HttpRequest ByteString -> m (HttpResponse ByteString)

jsonHttpRequest
  :: (Functor m, MonadHttp m, ToJSON input, FromJSON output)
  => HttpRequest input -> m (HttpResponse (Either String output))
jsonHttpRequest request =
  (eitherDecode <$>) <$> httpRequest (encode <$> request)

instance MonadHttp IO where
  httpRequest request = do
    let opts = defaults & checkResponse .~ Just trivialStatusChecker
    let method =
          case request ^. hrMethod of
            HttpMethodPost -> postWith
            HttpMethodPut -> putWith
    let methodString =
          case request ^. hrMethod of
            HttpMethodPost -> "POST"
            HttpMethodPut -> "PUT"
    response <-
      liftIO $
      method opts (Text.unpack (request ^. hrUrl)) (request ^. hrContent)
    let responseCode = response ^. responseStatus . statusCode
    return (HttpResponse responseCode (response ^. responseBody))
