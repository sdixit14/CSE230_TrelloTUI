module Types where
import Data.Text
import Data.Aeson
import GHC.Generics
import Lens.Micro.TH (makeLenses)

type User = Text                     -- username of the user 

data Workspaces = Workspaces {
    _allworkspaces :: [Workspaces]   -- list of all workspaces in the file
} deriving (Show,Generic, Eq)

data Workspace = Workspace {
    _name     :: Text,               -- name of the workspace
    _contents :: WorkspaceData       -- JSON object with users and tasks in given workspace
} deriving (Show, Generic, Eq)

instance ToJSON Workspace where
instance FromJSON Workspace where

data WorkspaceData = WorkspaceData {
    _users :: [User],                -- list of all users in the given workspace
    _tasks :: [Task]                 -- list of all tasks in the given workspace
} deriving (Show, Generic, Eq)

instance ToJSON WorkspaceData where
instance FromJSON WorkspaceData where

data Task = Task {
    _title    :: Text,               -- title of the task
    _content  :: Text,               -- description the task
    _assignee :: User                -- user to whom the task is assigned
} deriving (Show, Generic, Eq)

instance ToJSON Task where
instance FromJSON Task where

data Choice = Create | Join | Save | Cancel deriving (Show)
data Name = TitleField | ContentField deriving (Show,Eq,Generic,Ord)

data AppState e n = AppState {
    _workspaces      :: [Workspace], -- list of all workspaces in the file
    _workspace       :: Text,        -- name of the current workspace
    _user            :: User,        -- current user
    _showDialog      :: Bool,        -- True in Initial State, False otherwise
    _persistFile     :: FilePath     -- path of the file where data is persisted
}

makeLenses ''Workspaces
makeLenses ''Workspace
makeLenses ''WorkspaceData
makeLenses ''Task
makeLenses ''AppState