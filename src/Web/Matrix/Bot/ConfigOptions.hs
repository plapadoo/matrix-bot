{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Matrix.Bot.ConfigOptions(
    ConfigOptions(..)
  , readConfigOptions
  , coListenPort
  , coListenHost
  , coMatrix
  , coMatrixUserName
  , coMatrixPassword
  , coMatrixBasePath)
  where

import           Control.Lens    (Getter, to)
import           Data.Function   ((.))
import           Data.Functor    ((<$>))
import           Data.Int        (Int)
import           Data.Maybe      (Maybe)
import           Data.String     (String, fromString)
import qualified Data.Text       as Text
import qualified Dhall           as Dhall
import           GHC.Generics    (Generic)
import           Plpd.Dhall      (toString, toText)
import           Prelude         (fromIntegral)
import           System.FilePath
import           System.IO       (IO)

data MatrixOptions = MatrixOptions {
    userName :: Dhall.Text
  , basePath :: Dhall.Text
  , password :: Dhall.Text
  } deriving(Generic,Dhall.Interpret)

data ConfigOptions = ConfigOptions
    { listenPort :: Dhall.Natural
    , listenHost :: Maybe Dhall.Text
    , matrix     :: MatrixOptions
    } deriving(Generic,Dhall.Interpret)

coListenHost :: Getter ConfigOptions (Maybe Text.Text)
coListenHost = to ((toText <$>) . listenHost)

coMatrix :: Getter ConfigOptions MatrixOptions
coMatrix = to matrix

coMatrixUserName :: Getter MatrixOptions Text.Text
coMatrixUserName = to (toText . userName)

coMatrixPassword :: Getter MatrixOptions Text.Text
coMatrixPassword = to (toText . password)

coMatrixBasePath :: Getter MatrixOptions Text.Text
coMatrixBasePath = to (toText . basePath)

coListenPort :: Getter ConfigOptions Int
coListenPort = to (fromIntegral . listenPort)

readConfigOptions :: String -> IO ConfigOptions
readConfigOptions = Dhall.detailed . Dhall.input Dhall.auto . fromString
