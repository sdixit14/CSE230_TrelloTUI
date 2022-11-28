module Actions where

import Types
import Form
import System.Directory
import Brick.Types
import Brick.Forms
import Brick.Widgets.Core
import Brick.Widgets.Dialog  as D
import Data.Aeson                  (eitherDecode, encodeFile)
import Data.ByteString.Char8 as B  (pack)
import Data.ByteString.Lazy  as BL (fromStrict)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (zoom, (.=), (%=))

{-
  Upon application launch, fetch the list of workspaces present in the file which is parsed.
  Update the initial state of the application
-}

onStart path = do
  exists <- doesFileExist path
  if exists then do
    contents <- readFile path
    let response = eitherDecode ((BL.fromStrict . B.pack) contents) :: Either String [Workspace]
    case response of
      Left _           -> return $ blankAppState [] path
      Right workspaces -> return $ blankAppState workspaces path
  else do return $ blankAppState [] path
  

blankAppState :: [Workspace] -> FilePath -> AppState e Name
blankAppState workspaces path = AppState{
                            _workspaces    = workspaces,
                            _workspace     = "",
                            _user          = "",
                            _dialogFlag    = True,
                            _taskFormFlag  = False,
                            _workspaceFormFlag = False,
                            _dlg           = getDialog,
                            _taskForm      = emptyTaskForm,
                            _workspaceForm = emptyWorkspaceForm,
                            _persistFile   = path
                          }

displayDialog :: AppState e Name -> Widget Name
displayDialog st = padLeft (Pad 0) $ padRight Max $ padBottom Max dlgContent
    where dlgContent = D.renderDialog (st^.dlg) $ str "What would you like to do?"

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Select one") (Just (0, choices)) 50
    where
        choices = [ ("Create Workspace", Create)
                  , ("Join Workspace", Join)
                  ]
      
getFormFields :: Form FormFields e n -> FormFields
getFormFields f = FormFields{
  _wname = formState f^.wname,
  _username = formState f^.username
} 

createNewWorkspace :: AppState e n -> Workspace
createNewWorkspace st = Workspace {
  _name = st^.workspace,
  _users = [st^.user],
  _tasks = []
}
              