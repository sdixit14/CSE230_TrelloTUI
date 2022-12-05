module Types where
import Data.Text
import Data.Aeson
import GHC.Generics
import Brick.Widgets.Dialog
import Brick.Forms
import Lens.Micro.TH (makeLenses)

type User = Text                     -- username of the user 

data Workspaces = Workspaces {
    _allworkspaces :: [Workspaces]   -- list of all workspaces in the file
} deriving (Show,Generic, Eq)

data Workspace = Workspace {
    _name  :: Text,                  -- name of the workspace
    _users :: [User],                -- list of all users in the given workspace
    _tasks :: [Task]                 -- list of all tasks in the given workspace
} deriving (Show, Generic, Eq)

instance ToJSON Workspace where
instance FromJSON Workspace where

data Task = Task {
    _title    :: Text,               -- title of the task
    _content  :: Text,               -- description the task
    _assignee :: User                -- user to whom the task is assigned
} deriving (Show, Generic, Eq)

instance ToJSON Task where
instance FromJSON Task where

data FormFields = FormFields {
    _wname    :: Text,
    _username :: Text
} deriving (Show, Generic, Eq)

data Choice   = Create | Join deriving (Show, Eq, Generic, Ord)
data Name     = TitleField | ContentField | AssigneeField | NameField 
               | RadioField User | UserField deriving (Show, Eq, Generic, Ord)

data AppState e n= AppState {
    _workspaces        :: [Workspace], -- list of all workspaces in the file
    _workspace         :: Text,        -- name of the current workspace
    _user              :: User,        -- current user
    _dialogFlag        :: Bool,        -- True in Initial State, False otherwise
    _taskFormFlag      :: Bool,        -- False in Initial State, True upon Create Task
    _workspaceFormFlag :: Bool,        -- False in Initial State, True upon Create or Join Workspace
    _listTasksFlag     :: Bool,        -- False in Intial State, True after user creates/joins workspace
    _filterTasksFlag   :: Bool,        -- False in Intial State, True upon the event of filter tasks
    _dlg               :: Dialog Choice,
    _taskForm          :: Form Task e n,
    _workspaceForm     :: Form FormFields e n,
    _persistFile       :: FilePath     -- path of the file where data is persisted
}

makeLenses ''Workspaces
makeLenses ''Workspace
makeLenses ''Task
makeLenses ''AppState
makeLenses ''FormFields