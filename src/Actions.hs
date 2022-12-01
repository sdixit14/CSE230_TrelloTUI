module Actions where

import Types
import Form
import System.Directory
import Brick.Main as M
import Brick.Types
import Brick.Forms
import Brick.Widgets.Core
import Brick.Widgets.Dialog  as D
import Data.Aeson                  (eitherDecode, encodeFile)
import Data.ByteString.Char8 as B  (pack)
import Data.ByteString.Lazy  as BL (fromStrict)
import Lens.Micro                  (each, (^.))
import Lens.Micro.Mtl              (zoom, use, (.=), (%=))
import Control.Monad.IO.Class      (liftIO)
import Graphics.Vty          as V

onStart path = do
  exists <- doesFileExist path
  if exists then do
    contents <- readFile path
    let response = eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Workspace]
    case response of
      Left _           -> return $ blankAppState [] path
      Right workspaces -> return $ blankAppState workspaces path
  else do return $ blankAppState [] path

onExit :: AppState e Name -> EventM Name (AppState e Name) ()
onExit st = do
  liftIO (encodeFile (st^.persistFile) $ st^.workspaces)
  M.halt

onEnter :: AppState e Name -> EventM Name (AppState e Name) ()
onEnter st = do
  if st^.dialogFlag
    then do
      taskFormFlag .= False
      dialogFlag .= False
      workspaceFormFlag .= True
     else return ()

onSave :: AppState e Name -> EventM Name (AppState e Name) ()
onSave st = do                                                
  if st^.workspaceFormFlag
    then do
      wf <- use workspaceForm                                                       
      workspace .= (getFormFields wf)^.wname
      user .= (getFormFields wf)^.username
      workspaceFormFlag .= False
      let selection = show(D.dialogSelection (st^.dlg))
      if selection == "Just Create"
        then get >>= onCreateWorkspace
        else get >>= onJoinWorkspace
  else get >>= onSaveTask
  st <- get
  let tasksList = getCurrentTaskList st
  if (null tasksList)
    then listTasksFlag .= False
    else listTasksFlag .= True                                                         

onCancel :: AppState e Name -> EventM Name (AppState e Name) ()
onCancel st = do
  dialogFlag .= False
  workspaceFormFlag .= False
  taskFormFlag .= False

onCreateWorkspace :: AppState e Name -> EventM Name (AppState e Name) ()
onCreateWorkspace st = workspaces %= (++[(createNewWorkspace st)])

onJoinWorkspace :: AppState e Name -> EventM Name (AppState e Name) ()
onJoinWorkspace st = zoom (workspaces.each) $ do 
  let usersList = getCurrentUserList st
  if ((st^.user) `elem` usersList)
    then users %= (++[])
    else users %= (++[st^.user])

onCreateTask :: AppState e Name -> EventM Name (AppState e Name) ()
onCreateTask st = do
  dialogFlag .= False
  workspaceFormFlag .= False
  taskFormFlag .= True
  let l = (getCurrentUserList st)
  taskForm .= mkTaskForm l Task{
                _title   = "",
                _content = "",
                _assignee = ""
              }

onSaveTask :: AppState e Name -> EventM Name (AppState e Name) ()
onSaveTask st = do
  taskFormFlag .= False
  tf <- use taskForm
  zoom (workspaces.each) $ do
    workspace_name <- use name
    if (st^.workspace == workspace_name)
      then tasks %= (++[createNewTask tf])
      else return ()

onTyping :: V.Event -> BrickEvent Name e -> AppState e Name -> EventM Name (AppState e Name) ()
onTyping e ev st = do
  if st^.dialogFlag
    then zoom dlg $ handleDialogEvent e 
    else if st^.workspaceFormFlag
      then zoom workspaceForm $ handleFormEvent ev
      else zoom taskForm $ handleFormEvent ev 

blankAppState :: [Workspace] -> FilePath -> AppState e Name
blankAppState workspaces path = AppState{
                            _workspaces    = workspaces,
                            _workspace     = "",
                            _user          = "",
                            _dialogFlag    = True,
                            _taskFormFlag  = False,
                            _workspaceFormFlag = False,
                            _listTasksFlag = False,
                            _filterTasksFlag = False,
                            _dlg           = getDialog,
                            _taskForm      = emptyTaskForm,
                            _workspaceForm = emptyWorkspaceForm,
                            _persistFile   = path
                          }

displayDialog :: AppState e Name -> Widget Name
displayDialog st = padLeft (Pad 0) $ padRight Max $ padBottom Max dlgContent
    where dlgContent = D.renderDialog (st^.dlg) $ str " "

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Select one") (Just (0, choices)) 50
    where
        choices = [ ("Create Workspace", Create)
                  , ("Join Workspace", Join)
                  ]
      
getFormFields :: Form FormFields e n -> FormFields
getFormFields f = FormFields{
  _wname    = formState f^.wname,
  _username = formState f^.username
} 

getCurrentUserList :: AppState e Name  -> [User]
getCurrentUserList st = _users (Prelude.head (Prelude.filter (\wkspace -> _name wkspace == _workspace st) (_workspaces st)))

getCurrentTaskList :: AppState e Name  -> [Task]
getCurrentTaskList st = _tasks (Prelude.head (Prelude.filter (\wkspace -> _name wkspace == _workspace st) (_workspaces st)))

createNewWorkspace :: AppState e n -> Workspace
createNewWorkspace st = Workspace {
  _name  = st^.workspace,
  _users = [st^.user],
  _tasks = []
}

createNewTask :: Form Task e n -> Task
createNewTask f = Task{
  _title    = formState f^.title,
  _content  = formState f^.content,
  _assignee = formState f^.assignee
} 