{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

import           Control.Applicative                  (Applicative, pure, (*>),
                                                       (<*>))
import           Control.Lens                         (has, makeLenses,
                                                       makePrisms, view, (&),
                                                       (.~), (^.))
import           Control.Monad                        (Monad, mapM_, return)
import           Control.Monad.Identity               (Identity)
import           Control.Monad.Loops                  (iterateUntil)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       runReaderT)
import           Control.Monad.Writer                 (MonadWriter, Writer (..),
                                                       runWriter, tell)
import           Data.Bool                            (not)
import           Data.Eq                              ((==))
import           Data.Foldable                        (any)
import           Data.Function                        (const, flip, (.))
import           Data.Functor                         (Functor, (<$>))
import           Data.Maybe                           (Maybe (..), isJust)
import           Data.Ord                             ((<=))
import           Data.Text                            (Text, isInfixOf,
                                                       isPrefixOf, length)
import           Data.Text.IO                         (putStrLn)
import           Lucid                                (Html, body_, div_)
import           Network.HTTP.Types.Status            (forbidden403, ok200)
import           Plpd.Util                            (none, textShow)
import           Prelude                              (undefined)
import           System.FilePath                      (FilePath)
import           System.IO                            (IO)
import           Test.Framework                       (defaultMain)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (defaultMainGenerator)
import           Test.HUnit                           (Test (..), assertBool,
                                                       assertEqual,
                                                       assertFailure, runTestTT,
                                                       (@=?))
import           Test.QuickCheck                      (Arbitrary (..), (==>))
import           Test.QuickCheck.Instances
import           Text.Show                            (Show)
import           Web.Matrix.API                       (MatrixContext (..),
                                                       MatrixJoinReply (..),
                                                       MatrixJoinRequest (..),
                                                       MatrixLoginReply (..),
                                                       MatrixLoginRequest (..),
                                                       MatrixSendMessageReply (..),
                                                       MatrixSendMessageRequest (..))
import           Web.Matrix.Bot.IncomingMessage       (IncomingMessage, bodyEnd,
                                                       bodyStart,
                                                       constructIncomingMessage,
                                                       coparseIncomingMessage,
                                                       markupBody,
                                                       parseIncomingMessage,
                                                       plainBody)
import           Web.Matrix.Bot.WebServer             (WebServerInput (..),
                                                       WebServerOutput (..),
                                                       handleMessage)

data MockOperations
    = OperationPutLog Text
    | OperationMatrixLogin MatrixLoginRequest
    | OperationMatrixJoin MatrixJoinRequest
    | OperationMatrixSendMessage MatrixSendMessageRequest
    deriving ((Show))

makePrisms ''MockOperations

data MatrixProcessors = MatrixProcessors
    { _mpLogin       :: MatrixLoginRequest -> MatrixLoginReply
    , _mpJoin        :: MatrixJoinRequest -> MatrixJoinReply
    , _mpSendMessage :: MatrixSendMessageRequest -> MatrixSendMessageReply
    }

loginNoError = const (MatrixLoginReply Nothing Nothing (Just "accesstoken"))

joinNoError = const (MatrixJoinReply Nothing Nothing)

sendMessageNoError = const (MatrixSendMessageReply Nothing Nothing)

processorsNoError =
    MatrixProcessors loginNoError joinNoError sendMessageNoError

makeLenses ''MatrixProcessors

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

-- Not really arbitrary yet
instance Arbitrary (Html ()) where
  arbitrary = do
    return (body_ (div_ "foo"))

instance Arbitrary (IncomingMessage Text (Html ())) where
    arbitrary = do
        plain <- arbitrary
        doMarkup <- arbitrary
        if doMarkup
            then do
                markup <- arbitrary
                return (constructIncomingMessage plain (Just markup))
            else return (constructIncomingMessage plain Nothing)

-- Not applicable anymore since parsing results in text, not HTML
-- prop_coparseParseAreInverse message = do
--     (parseIncomingMessage . coparseIncomingMessage) message == message

-- prop_parseCoparseAreInverse message = do
--     (coparseIncomingMessage . parseIncomingMessage) message == message

prop_coparseWithMarkupContainsBodyStart message = do
    isJust (message ^. markupBody) ==> bodyStart `isPrefixOf`
        coparseIncomingMessage message

prop_coparseWithMarkupContainsBodyEnd message = do
    isJust (message ^. markupBody) ==> bodyEnd `isInfixOf`
        coparseIncomingMessage message

main :: IO ()
main = $(defaultMainGenerator)
