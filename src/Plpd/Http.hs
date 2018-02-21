{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Plpd.Http
  ( HttpMethod(..)
  , HttpRequest(..)
  , HttpResponse(..)
  , MonadHttp(..)
  , loggingHttp
  , jsonHttpRequest
  , hrUrl
  , hrMethod
  , hrContentType
  , hrContent
  , hresStatusCode
  , hresContent
  ) where

import           Control.Applicative        (pure)
import           Control.Lens               (makeLenses, to, view, (&), (.~),
                                             (?~), (^.))
import           Control.Monad              (Monad)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             eitherDecode, encode, toJSON)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Either                (Either)
import           Data.Function              (const, ($), (.))
import           Data.Functor               (Functor, (<$>))
import           Data.Int                   (Int)
import           Data.Monoid                ((<>))
import           Data.String                (String)
import qualified Data.Text                  as Text
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.UUID                  (UUID, toText)
import           Network.Wreq               (checkResponse, defaults, header,
                                             linkURL, param, postWith, putWith,
                                             responseBody, responseLink,
                                             responseStatus, statusCode)
import           Network.Wreq.Types         (ResponseChecker)
import           Plpd.MonadLog              (MonadLog (..))
import           Plpd.Util                  (textShow)
import           System.IO                  (IO)
import           System.Random              (randomIO)
import           Text.Show                  (Show, show)

-- HTTP API abstraction
data HttpMethod
  = HttpMethodPost
  | HttpMethodPut

instance Show HttpMethod where
  show HttpMethodPost = "POST"
  show HttpMethodPut  = "PUT"

data HttpRequest a = HttpRequest
  { _hrUrl         :: Text.Text
  , _hrMethod      :: HttpMethod
  , _hrContentType :: Text.Text
  , _hrContent     :: a
  } deriving (Functor)

makeLenses ''HttpRequest

data HttpResponse a = HttpResponse
  { _hresStatusCode :: Int
  , _hresContent    :: a
  } deriving (Functor)

trivialStatusChecker :: ResponseChecker
trivialStatusChecker _ _ = pure ()

makeLenses ''HttpResponse

class MonadHttp m where
  httpRequest :: HttpRequest ByteString -> m (HttpResponse ByteString)

jsonHttpRequest
  :: (Functor m, MonadHttp m, ToJSON input, FromJSON output)
  => HttpRequest input -> m (HttpResponse (Either String output))
jsonHttpRequest request =
  (eitherDecode <$>) <$> httpRequest (encode <$> request)

loggingHttp :: (MonadLog m,MonadIO m,Monad m) => HttpRequest ByteString -> m (HttpResponse ByteString)
loggingHttp request = do
        uuid <- toText <$> liftIO randomIO
        putLog $ uuid <> ": HTTP " <> textShow (request ^. hrMethod) <>
            " request to " <>
            (request ^. hrUrl) <>
            ", content type " <>
            (request ^. hrContentType) <>
            ", content: " <>
            (toStrict . decodeUtf8) (request ^. hrContent)
        response <- liftIO (httpRequest request)
        putLog $ uuid <> ": response code " <>
            textShow (response ^. hresStatusCode) <>
            ", content: " <>
            (toStrict . decodeUtf8) (response ^. hresContent)
        pure response

instance MonadHttp IO where
  httpRequest request = do
    let opts = defaults & checkResponse ?~ trivialStatusChecker
    let method =
          case request ^. hrMethod of
            HttpMethodPost -> postWith
            HttpMethodPut  -> putWith
    let methodString =
          case request ^. hrMethod of
            HttpMethodPost -> "POST"
            HttpMethodPut  -> "PUT"
    response <-
      liftIO $
      method opts (Text.unpack (request ^. hrUrl)) (request ^. hrContent)
    let responseCode = response ^. responseStatus . statusCode
    pure (HttpResponse responseCode (response ^. responseBody))
