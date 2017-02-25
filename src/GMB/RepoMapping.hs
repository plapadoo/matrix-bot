{-# LANGUAGE OverloadedStrings #-}
module GMB.RepoMapping(
    RepoMapping
  , readRepoMapping
  , RoomEntity(..)
  , Room(..)
  , Directory(..)
  , Repo(..)
  , roomsForEntity
  , roomToDirs
  , rooms) where

import           Control.Applicative    ((*>), (<*), (<*>))
import           Control.Monad          (return)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text   as Atto
import           Data.Either            (Either)
import           Data.Eq                ((/=), (==),Eq)
import           Data.Foldable          (foldMap,concatMap)
import           Data.Functor           ((<$>))
import Data.Char(Char)
import Data.Bool(Bool,(||),otherwise)
import Data.Function((.))
import           Data.Map               (Map, keys,singleton,toList)
import           Data.Ord               (Ord)
import Data.List(filter)
import Data.Tuple(fst,snd)
import           Data.String            (String)
import           Data.Text              (Text,isPrefixOf,drop)
import           Data.Text.IO           (readFile)
import           Prelude                (undefined)
import           System.FilePath(FilePath)
import           Text.Show              (Show)

data Room = Room Text deriving(Ord,Eq,Show)

data Repo = Repo Text deriving(Ord,Eq,Show)

data Directory = Directory Text deriving(Ord,Eq,Show)

data RoomEntity = RoomToRepo Repo | RoomToDirectory Directory deriving(Eq,Show)

type RepoMapping = Map Room [RoomEntity]

rooms :: RepoMapping -> [Room]
rooms rm = keys rm

repoMappingToList :: RepoMapping -> [(Room,RoomEntity)]
repoMappingToList rm = [ (room,repo) | (room,repos) <- toList rm,repo <- repos ]

roomsForEntity :: RepoMapping -> RoomEntity -> [Room]
roomsForEntity rm repo = fst <$> (filter ((==repo) . snd) (repoMappingToList rm))

roomToDirs :: RepoMapping -> [(Room,Directory)]
roomToDirs rm = concatMap (\(room,entity) -> case entity of RoomToDirectory dir -> [(room,dir)]; _ -> []) (repoMappingToList rm)

data RepoMappingLine = RepoMappingLine Text [Text]

repoOrDir text | "@" `isPrefixOf` text = RoomToDirectory (Directory (drop 1 text))
               | otherwise = RoomToRepo (Repo text)

linesToMapping :: [RepoMappingLine] -> RepoMapping
linesToMapping = foldMap (\(RepoMappingLine key values) -> singleton (Room key) (repoOrDir <$> values))

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t'

attoSkipSpace = Atto.skipWhile isSpace

lineParser :: Atto.Parser RepoMappingLine
lineParser =
  RepoMappingLine <$> (attoSkipSpace *> (Atto.takeWhile (/= '=') <* Atto.char '=') )
                  <*> ((attoSkipSpace *> Atto.takeWhile (Atto.notInClass ",\n") <* attoSkipSpace) `Atto.sepBy` (Atto.char ','))

fileParser :: Atto.Parser [RepoMappingLine]
fileParser = lineParser `Atto.sepBy` Atto.char '\n'

readRepoMapping :: MonadIO m => FilePath -> m (Either String RepoMapping)
readRepoMapping fileName = do
  file <- liftIO (readFile fileName)
  return (linesToMapping <$> (Atto.parseOnly fileParser file))
