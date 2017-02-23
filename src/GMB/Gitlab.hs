{-# LANGUAGE DeriveGeneric #-}
module GMB.Gitlab(
  GitlabEvent,
  GitlabCommit,
  GitlabRepository,
  eventRepository,
  commitMessage,
  eventUserName,
  eventCommits,
  repositoryName) where

import Prelude()
import GHC.Generics(Generic)
import Data.Maybe(Maybe)
import Data.Text(Text)
import Data.Aeson(FromJSON)

data GitlabRepository = GitlabRepository {
    name :: Text
  } deriving(Generic)

data GitlabCommit = GitlabCommit {
    message :: Text
  } deriving(Generic)

commitMessage :: GitlabCommit -> Text
commitMessage = message

data GitlabEvent = GitlabEvent {
    object_kind :: Text
  , user_name :: Maybe Text
  , repository :: Maybe GitlabRepository
  , commits :: Maybe [GitlabCommit]
  } deriving(Generic)

eventCommits :: GitlabEvent -> Maybe [GitlabCommit]
eventCommits = commits

eventRepository :: GitlabEvent -> Maybe GitlabRepository
eventRepository = repository

eventUserName :: GitlabEvent -> Maybe Text
eventUserName = user_name

repositoryName :: GitlabRepository -> Text
repositoryName = name

instance FromJSON GitlabCommit
instance FromJSON GitlabRepository
instance FromJSON GitlabEvent
