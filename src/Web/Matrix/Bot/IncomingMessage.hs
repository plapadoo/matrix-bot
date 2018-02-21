{-# LANGUAGE TupleSections #-}

module Web.Matrix.Bot.IncomingMessage
  ( parseIncomingMessage
  , coparseIncomingMessage
  , constructIncomingMessage
  , constructIncomingMessageLazy
  , bodyStart
  , bodyEnd
  , IncomingMessage
  , plainBody
  , markupBody)
  where

import           Control.Lens   (Getter, Lens', Prism', prism', to, _1, _2)
import           Data.Bifunctor (Bifunctor (..))
import           Data.Eq        (Eq)
import           Data.Foldable  (foldMap)
import           Data.Function  (id, ($), (.))
import           Data.Functor   ((<$>))
import           Data.Maybe     (Maybe (..), maybe)
import           Data.Monoid    ((<>))
import qualified Data.Text      as Text
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as TextLazy
import           Lucid          (Html, body_, renderText)
import           Plpd.Util      (breakOnMaybe)
import           Text.Show      (Show)

data IncomingMessage a b =
    IncomingMessage (a, Maybe b)
    deriving (Eq,Show)

instance Bifunctor IncomingMessage where
  first f (IncomingMessage (a,b)) = IncomingMessage ((f a),b)
  second f (IncomingMessage (a,b)) = IncomingMessage (a,f <$> b)

plainBody :: Lens' (IncomingMessage a b) a
plainBody f (IncomingMessage (t,u)) = (IncomingMessage . (, u)) <$> f t

markupBody :: Lens' (IncomingMessage a b) (Maybe b)
markupBody f (IncomingMessage (u,t)) = (IncomingMessage . (u, )) <$> f t

bodyStart = "<body>"

bodyEnd = "</body>"

parseIncomingMessage :: Text.Text -> (IncomingMessage Text.Text Text.Text)
parseIncomingMessage mb =
    case breakOnMaybe bodyStart mb of
        Nothing -> IncomingMessage (mb, Nothing)
        Just (_,bodyText) ->
            case breakOnMaybe bodyEnd bodyText of
                Nothing -> IncomingMessage (mb, Nothing)
                Just (bodyTextTillEnd,bodyEnd) ->
                    IncomingMessage
                        ( Text.drop 7 bodyEnd
                        , Just (Text.drop 6 bodyTextTillEnd))

constructIncomingMessage :: a -> Maybe b -> IncomingMessage a b
constructIncomingMessage plain markup = IncomingMessage (plain, markup)

constructIncomingMessageLazy :: TextLazy.Text -> Maybe TextLazy.Text -> (IncomingMessage Text.Text Text.Text)
constructIncomingMessageLazy plain markup = IncomingMessage (TextLazy.toStrict plain, TextLazy.toStrict <$> markup)

coparseIncomingMessage :: (IncomingMessage Text.Text (Html ())) -> Text.Text
coparseIncomingMessage (IncomingMessage (plain,markup)) = (foldMap id (toStrict . renderText . body_ <$> markup)) <> plain
