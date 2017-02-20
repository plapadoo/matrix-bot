{-# LANGUAGE OverloadedStrings #-}
module GMB.RepoMapping(
    RepoMapping
  , readRepoMapping
  , Room(..)
  , Repo(..)
  , roomsForRepo
  , rooms) where

import           Control.Applicative    ((*>), (<*), (<*>))
import           Control.Monad          (return)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text   as Atto
import           Data.Either            (Either)
import           Data.Eq                ((/=), (==),Eq)
import           Data.Foldable          (foldMap)
import           Data.Functor           ((<$>))
import Data.Function((.))
import           Data.Map               (Map, keys,singleton,toList)
import           Data.Ord               (Ord)
import Data.List(filter)
import Data.Tuple(fst,snd)
import           Data.String            (String)
import           Data.Text              (Text)
import           Data.Text.IO           (readFile)
import           Prelude                (undefined)
import           System.FilePath(FilePath)
import           Text.Show              (Show)

data Room = Room Text deriving(Ord,Eq,Show)

data Repo = Repo Text deriving(Ord,Eq,Show)

type RepoMapping = Map Room [Repo]

rooms :: RepoMapping -> [Room]
rooms rm = keys rm

repoMappingToList :: RepoMapping -> [(Room,Repo)]
repoMappingToList rm = [ (room,repo) | (room,repos) <- toList rm,repo <- repos ]

roomsForRepo :: RepoMapping -> Repo -> [Room]
roomsForRepo rm repo = fst <$> (filter ((==repo) . snd) (repoMappingToList rm))

data RepoMappingLine = RepoMappingLine Text [Text]

linesToMapping :: [RepoMappingLine] -> RepoMapping
linesToMapping = foldMap (\(RepoMappingLine key values) -> singleton (Room key) (Repo <$> values))

lineParser :: Atto.Parser RepoMappingLine
lineParser =
  RepoMappingLine <$> (Atto.skipSpace *> (Atto.takeWhile (/= '=') <* Atto.char '=') )
                  <*> ((Atto.skipSpace *> Atto.takeWhile (Atto.notInClass ",\n") <* Atto.skipSpace) `Atto.sepBy` (Atto.char ','))

fileParser :: Atto.Parser [RepoMappingLine]
fileParser = lineParser `Atto.sepBy` Atto.char '\n'

readRepoMapping :: MonadIO m => FilePath -> m (Either String RepoMapping)
readRepoMapping fileName = do
  file <- liftIO (readFile fileName)
  return (linesToMapping <$> (Atto.parseOnly fileParser file))
