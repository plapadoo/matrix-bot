{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Plpd.Http
  ( HttpMethod(..)
  , HttpRequest(..)
  , HttpResponse(..)
  , loggingHttp
  , jsonHttpRequest
  , hrUrl
  , hrMethod
  , hrContentType
  , hrContent
  , hresStatusCode
  , hresContent
  , JsonParseError
  , jpeOriginal
  , jpeError
  ) where

import           Control.Applicative     (pure)
import           Control.Lens            (makeLenses, (&), (?~), (^.))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          eitherDecode, encode)
import           Data.Bifunctor          (first)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Either             (Either)
import           Data.Function           (($), (.))
import           Data.Functor            (Functor, (<$>))
import           Data.Int                (Int)
import           Data.Monoid             ((<>))
import           Data.String             (String)
import qualified Data.Text               as Text
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.UUID               (toText)
import           Network.Wreq            (checkResponse, defaults, postWith,
                                          putWith, responseBody, responseStatus,
                                          statusCode)
import           Network.Wreq.Types      (ResponseChecker)
import           Plpd.MonadLog           (LogMode (LogStdout), defaultLog)
import           Plpd.Util               (textShow)
import           System.IO               (IO)
import           System.Random           (randomIO)
import           Text.Show               (Show, show)

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

-- eitherDecode returns an error, but keeping the original reply is even better.
data JsonParseError = JsonParseError {
    _jpeOriginal :: ByteString
  , _jpeError    :: String
  }

makeLenses ''JsonParseError

eitherDecodeKeep :: FromJSON a => ByteString -> Either JsonParseError a
eitherDecodeKeep original = first (JsonParseError original) (eitherDecode original)

jsonHttpRequest
  :: (ToJSON input, FromJSON output, MonadIO m)
  => HttpRequest input -> m (HttpResponse (Either JsonParseError output))
jsonHttpRequest request =
  (eitherDecodeKeep <$>) <$> httpRequest (encode <$> request)

loggingHttp :: HttpRequest ByteString -> IO (HttpResponse ByteString)
loggingHttp request = do
  uuid <- toText <$> randomIO
  defaultLog LogStdout $ uuid <> ": HTTP " <> textShow (request ^. hrMethod) <>
      " request to " <>
      (request ^. hrUrl) <>
      ", content type " <>
      (request ^. hrContentType) <>
      ", content: " <>
      (toStrict . decodeUtf8) (request ^. hrContent)
  response <- (httpRequest request)
  defaultLog LogStdout $ uuid <> ": response code " <>
      textShow (response ^. hresStatusCode) <>
      ", content: " <>
      (toStrict . decodeUtf8) (response ^. hresContent)
  pure response

httpRequest :: MonadIO m => HttpRequest ByteString -> m (HttpResponse ByteString)
httpRequest request = do
    let opts = defaults & checkResponse ?~ trivialStatusChecker
    let method =
          case request ^. hrMethod of
            HttpMethodPost -> postWith
            HttpMethodPut  -> putWith
    response <- liftIO $
                method opts (Text.unpack (request ^. hrUrl)) (request ^. hrContent)
    let responseCode = response ^. responseStatus . statusCode
    pure (HttpResponse responseCode (response ^. responseBody))
