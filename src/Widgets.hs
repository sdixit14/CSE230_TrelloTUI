module Widgets where

import Types
import Actions
import Form
import Brick.AttrMap as A
import Brick.Main                 as M
import Brick.Types
import Brick.Forms as F
import Brick.Util                 (on,bg)
import Brick.Widgets.Border       (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core
import Brick.Widgets.Dialog  as D
import Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)
import Data.List                  as L (elem, intercalate)
import Data.Text                  as T hiding (center, null)
import Graphics.Vty               as V
import Lens.Micro                 (each, (%~), (&), (.~), (^.), (^?))
import Lens.Micro.Mtl (preview, zoom, use, (.=), (%=))
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

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
                -- display all tasks in the given workspace
                | otherwise = center (txt ( ((((st^.workspaces)!!0) ^.users)!!0)) )
                -- | otherwise = center (txt (st^.workspace))


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

appEvent :: BrickEvent Name e -> EventM Name (AppState e Name) ()
appEvent ev = 
  case ev of
    (VtyEvent (V.EvKey V.KEsc  []))              -> M.halt -- TODO: call onExit and then Halt
    (VtyEvent (V.EvKey V.KEnter  []))            -> do
                                                    st <- get
                                                    if st^.dialogFlag
                                                      then do
                                                        taskFormFlag .= False
                                                        dialogFlag .= False
                                                        workspaceFormFlag .= True
                                                    else return ()
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
                                                        else return ()              
                                                    else return ()

    (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> do
                                                    dialogFlag .= False
                                                    workspaceFormFlag .= False
                                                    taskFormFlag .= False
    (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) -> do
                                                    dialogFlag .= False
                                                    workspaceFormFlag .= False
                                                    taskFormFlag .= True
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
