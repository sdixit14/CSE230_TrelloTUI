module Types where
import Data.Text
import Data.Aeson
import GHC.Generics

type User = Text

data CmdInputOptions = FileInput FilePath

data Workspace = Workspace {
    _name :: Text,
    _users :: [User],
    _tasks :: [Task]
} deriving (Show, Generic, Eq)

instance ToJSON Workspace where
instance FromJSON Workspace where

data Task = Task {
    _title :: Text,
    _content :: Text,
    _assignee :: User
} deriving (Show, Generic, Eq)

instance ToJSON Task where
instance FromJSON Task where