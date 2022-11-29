module Widgets where

import Types
import Actions
import Form
import Tasks
import Data.Aeson                 (encodeFile)
import Brick.AttrMap              as A
import Brick.Main                 as M
import Brick.Types
import Brick.Forms                as F
import Control.Monad.IO.Class     (liftIO)
import Brick.Util                 (on,bg)
import Brick.Widgets.Border       (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core
import Brick.Widgets.Dialog       as D
import Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)
import Data.List                  as L (elem, intercalate)
import Data.Text                  as T hiding (center, null)
import Graphics.Vty               as V
import Lens.Micro                 (each, (%~), (&), (.~), (^.), (^?), (^..), filtered)
import Lens.Micro.Mtl (preview, zoom, use, (.=), (%=))

helpText = [  "Ctrl-n         : Create Task",
              "Ctrl-s         : Save & close dialog",
              "Ctrl-c         : Cancel Dialog",
              "Esc            : Save State & Exit",
              "F1             : Filter Tasks Assigned to Me"
          ]

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.dialogFlag        = displayDialog st
                | st^.workspaceFormFlag = getWorkspaceForm (st^.workspaceForm)
                | st^.taskFormFlag      = getTaskForm (st^.taskForm)
                | st^.listTasksFlag     = scrollabletaskWidget st
                -- display all tasks in the given workspace
                | otherwise = welcomeWidget


welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to Trello-TUI")

helpWidget :: AppState e Name -> Widget Name
helpWidget st = result
  where result | not(st^.dialogFlag) = borderWithLabel (str "Help") $
                                       hLimitPercent 15  $
                                       vLimitPercent 100 $
                                       padBottom Max
                                       (txtWrap $ T.pack $ L.intercalate "\n" helpText)
               | otherwise = emptyWidget

getCurrentUserList :: AppState e Name  -> [User]
getCurrentUserList st = _users (Prelude.head (Prelude.filter (\wkspace -> _name wkspace == _workspace st) (_workspaces st)))

getCurrentTaskList :: AppState e Name  -> [Task]
getCurrentTaskList st = _tasks (Prelude.head (Prelude.filter (\wkspace -> wkspace^.name == st^.workspace) (st^.workspaces)))

appEvent :: BrickEvent Name e -> EventM Name (AppState e Name) ()
appEvent ev = 
  case ev of
    -- Update the file with the current state and exit application
    (VtyEvent (V.EvKey V.KEsc  []))              -> do
                                                    st <- get
                                                    liftIO (encodeFile (st^.persistFile) $ st^.workspaces)
                                                    M.halt
    -- In the case of Create/Join dialog box, render the Create/Join Workspace form
    (VtyEvent (V.EvKey V.KEnter  []))            -> do
                                                    st <- get
                                                    if st^.dialogFlag
                                                      then do
                                                        taskFormFlag .= False
                                                        dialogFlag .= False
                                                        workspaceFormFlag .= True
                                                       else return ()
    -- Update the current user and current workspace in AppState based on the workspace form entries
    -- If Dialog Selection is to Create Workspace then add a new workspace object in the list of workspaces
    -- TODO : If Dialog Selection is to Join Workspace then append a new user to the list of users in the given workspace (identified through the current workspace)
    (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) -> do
                                                    st <- get                                                  
                                                    if st^.workspaceFormFlag
                                                      then do
                                                        wf <- use workspaceForm                                                       
                                                        workspace .= (getFormFields wf)^.wname
                                                        user .= (getFormFields wf)^.username
                                                        workspaceFormFlag .= False
                                                        let selection = show(D.dialogSelection (st^.dlg))
                                                        if selection == "Just Create"
                                                          then do
                                                            st <- get
                                                            workspaces %= (++[(createNewWorkspace st)])

                                                        else do
                                                          st <- get
                                                          zoom (workspaces.each) $ do
                                                            workspace_name <- use name
                                                            if (st^.workspace == workspace_name)
                                                              then do 
                                                                let usersList = getCurrentUserList st
                                                                if ((st^.user) `elem` usersList)
                                                                  then users %= (++[])
                                                                else users %= (++[st^.user])
                                                            else users %= (++[])
                                                        st <- get
                                                        let tasksList = getCurrentTaskList st
                                                        if (null tasksList)
                                                          then listTasksFlag .= False
                                                        else listTasksFlag .= True                                                         
                                                      else return ()
    -- Turn off all flags and do not update state
    (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> do
                                                    dialogFlag .= False
                                                    workspaceFormFlag .= False
                                                    taskFormFlag .= False
    -- Turn on the task form flag which would display the Create Task form through drawUI
    (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) -> do
                                                      st <- get
                                                      dialogFlag .= False
                                                      workspaceFormFlag .= False
                                                      taskFormFlag .= True
                                                      let l = (getCurrentUserList st)
                                                      taskForm .= mkTaskForm l Task{
                                                                    _title   = "",
                                                                    _content = "",
                                                                    _assignee = ""
                                                                  } 
                                                      
    -- Handle dialog event if the dialog flag is set to True
    -- Handle form events if the workspace/form flag is set to True
    (VtyEvent e)                                 -> do
                                                    st <- get
                                                    if st^.dialogFlag
                                                      then zoom dlg $ handleDialogEvent e 
                                                      else if st^.workspaceFormFlag
                                                        then zoom workspaceForm $ handleFormEvent ev
                                                        else zoom taskForm $ handleFormEvent ev   

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      helpWidget st <+>
      borderWithLabel (str "Trello-TUI") (drawLayer st)
    ]

app :: M.App (AppState e Name) e Name
app =
    M.App { M.appDraw         = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return ()
          , M.appAttrMap      = const theMap
          }

theMap :: AttrMap
theMap = A.attrMap V.defAttr
   [  (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]
