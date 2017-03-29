{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative (Applicative, pure, (*>), (<*>))
import Control.Lens
       (has, makeLenses, makePrisms, view, (&), (.~), (^.))
import Control.Monad (Monad, mapM_, return)
import Control.Monad.Identity (Identity)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Writer
       (MonadWriter, Writer(..), runWriter, tell)
import Data.Bool (not)
import Data.Eq ((==))
import Data.Foldable (any)
import Data.Function (const, flip, (.))
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe(..), isJust)
import Data.Ord ((<=))
import Data.Text (Text, isInfixOf, isPrefixOf, length)
import Data.Text.IO (putStrLn)
import Web.Matrix.Bot.IncomingMessage
       (IncomingMessage, bodyEnd, bodyStart, constructIncomingMessage,
        coparseIncomingMessage, markupBody, parseIncomingMessage,
        plainBody)
import Web.Matrix.API
       (MatrixContext(..), MatrixJoinReply(..), MatrixJoinRequest(..),
        MatrixLoginReply(..), MatrixLoginRequest(..),
        MatrixSendMessageReply(..), MatrixSendMessageRequest(..),MonadMatrix(..))
import Plpd.MonadLog (MonadLog(..))
import Plpd.Util (none, textShow)
import Web.Matrix.Bot.WebServer
       (WebServerInput(..), WebServerOutput(..), handleMessage)
import Network.HTTP.Types.Status (forbidden403, ok200)
import Prelude (undefined)
import System.FilePath (FilePath)
import System.IO (IO)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit
       (Test(..), assertBool, assertEqual, assertFailure, runTestTT,
        (@=?))
import Test.QuickCheck (Arbitrary(..), (==>))
import Test.QuickCheck.Instances
import Text.Show (Show)

data MockOperations
    = OperationPutLog Text
    | OperationMatrixLogin MatrixLoginRequest
    | OperationMatrixJoin MatrixJoinRequest
    | OperationMatrixSendMessage MatrixSendMessageRequest
    deriving ((Show))

makePrisms ''MockOperations

data MatrixProcessors = MatrixProcessors
    { _mpLogin :: MatrixLoginRequest -> MatrixLoginReply
    , _mpJoin :: MatrixJoinRequest -> MatrixJoinReply
    , _mpSendMessage :: MatrixSendMessageRequest -> MatrixSendMessageReply
    }

loginNoError = const (MatrixLoginReply Nothing Nothing (Just "accesstoken"))

joinNoError = const (MatrixJoinReply Nothing Nothing)

sendMessageNoError = const (MatrixSendMessageReply Nothing Nothing)

processorsNoError =
    MatrixProcessors loginNoError joinNoError sendMessageNoError

makeLenses ''MatrixProcessors

newtype MyMonad a = MyMonad
    { runMyMonad :: ReaderT MatrixProcessors (Writer [MockOperations]) a
    } deriving (Functor,Applicative,Monad,MonadWriter [MockOperations],MonadReader MatrixProcessors)

instance MonadLog MyMonad where
    putLog text = tell [OperationPutLog text]

instance MonadMatrix MyMonad where
    login mc request =
        tell [OperationMatrixLogin request] *> view mpLogin <*> pure request
    joinRoom mc request =
        tell [OperationMatrixJoin request] *> view mpJoin <*> pure request
    sendMessage mc request =
        tell [OperationMatrixSendMessage request] *> view mpSendMessage <*>
        pure request

case_joinFails = do
    let (WebServerOutput _ status,logs) =
            (runWriter .
             (flip runReaderT)
                 (processorsNoError & mpJoin .~
                  (const (MatrixJoinReply (Just "foo") (Just "bar")))) .
             runMyMonad)
                (handleMessage
                     (WebServerInput
                          (MatrixContext "baseurl")
                          "accesstoken"
                          "room"
                          "<body>markup</body>simple"))
    mapM_ (putStrLn . textShow) logs
    assertBool "No join found! " (any (has _OperationMatrixJoin) logs)
    assertBool
        "Send message found! "
        (none (has _OperationMatrixSendMessage) logs)
    forbidden403 @=? status

case_allSucceeds = do
    let (WebServerOutput _ status,logs) =
            (runWriter . (flip runReaderT) processorsNoError . runMyMonad)
                (handleMessage
                     (WebServerInput
                          (MatrixContext "baseurl")
                          "accesstoken"
                          "room"
                          "<body>markup</body>simple"))
    mapM_ (putStrLn . textShow) logs
    assertBool "No join found! " (any (has _OperationMatrixJoin) logs)
    assertBool
        "No send message found! "
        (any (has _OperationMatrixSendMessage) logs)
    ok200 @=? status

case_parseMessageWithoutBody = do
    let input = "lol"
        result = parseIncomingMessage input
    input @=? result ^. plainBody
    Nothing @=? result ^. markupBody

case_parseMessageStartingWithBody = do
    let input = "<body>lol"
        result = parseIncomingMessage input
    input @=? result ^. plainBody
    Nothing @=? result ^. markupBody

case_parseMessageWithBodyAndMarkup = do
    let input = "<body>markup</body>nomarkup"
        result = parseIncomingMessage input
    "nomarkup" @=? result ^. plainBody
    Just "markup" @=? result ^. markupBody

instance Arbitrary IncomingMessage where
    arbitrary = do
        plain <- arbitrary
        doMarkup <- arbitrary
        if doMarkup
            then do
                markup <- iterateUntil (not . (bodyEnd `isInfixOf`)) arbitrary
                return (constructIncomingMessage plain (Just markup))
            else return (constructIncomingMessage plain Nothing)

prop_coparseParseAreInverse message = do
    (parseIncomingMessage . coparseIncomingMessage) message == message

prop_parseCoparseAreInverse message = do
    (coparseIncomingMessage . parseIncomingMessage) message == message

prop_coparseWithMarkupContainsBodyStart message = do
    isJust (message ^. markupBody) ==> bodyStart `isPrefixOf`
        coparseIncomingMessage message

prop_coparseWithMarkupContainsBodyEnd message = do
    isJust (message ^. markupBody) ==> bodyEnd `isInfixOf`
        coparseIncomingMessage message

main :: IO ()
main = $(defaultMainGenerator)
