{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Applicative                  ((<$>), (<*>))
import           Control.Lens                         ((^.))
import           Control.Monad                        (return)
import           Control.Monad.Loops                  (iterateUntil)
import           Data.Bool                            (not)
import           Data.Eq                              ((==))
import           Data.Function                        ((.))
import           Data.Maybe                           (Maybe (..),isJust)
import           Data.Ord                             ((<=))
import           Data.Text                            (isPrefixOf,isInfixOf, length)
import           GMB.IncomingMessage                  (IncomingMessage, bodyStart,bodyEnd,
                                                       constructIncomingMessage,
                                                       coparseIncomingMessage,
                                                       markupBody,
                                                       parseIncomingMessage,
                                                       plainBody)
import           Prelude                              ()
import           System.IO                            (IO)
import           Test.Framework                       (defaultMain)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (defaultMainGenerator)
import           Test.HUnit                           (Test (..), assertEqual,
                                                       assertFailure, runTestTT,
                                                       (@=?))
import           Test.QuickCheck                      (Arbitrary (..),(==>))
import           Test.QuickCheck.Instances

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
  isJust (message ^. markupBody) ==> bodyStart `isPrefixOf` coparseIncomingMessage message

prop_coparseWithMarkupContainsBodyEnd message = do
  isJust (message ^. markupBody) ==> bodyEnd `isInfixOf` coparseIncomingMessage message

main :: IO ()
main = $(defaultMainGenerator)
