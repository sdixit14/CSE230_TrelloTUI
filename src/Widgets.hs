module Widgets where

import Types
import Actions
import Form
import Tasks
import Data.Aeson                 (encodeFile)
import Brick.AttrMap              as A
import Brick.Main                 as M
import Brick.Types
import Brick.Forms
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
import Lens.Micro                 (each, (^.))
import Lens.Micro.Mtl              (zoom, use, (.=), (%=))

helpText = [  "Ctrl-n    : Create Task",
              "Ctrl-s    : Save & Close Dialog",
              "Ctrl-c    : Cancel Dialog",
              "Ctrl-f    : Tasks Assigned to Me",
              "Ctrl-a    : Remove Filter",
              "Esc       : Save State & Exit"
            ]

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.dialogFlag        = displayDialog st
                | st^.workspaceFormFlag = getWorkspaceForm (st^.workspaceForm)
                | st^.taskFormFlag      = getTaskForm (st^.taskForm)
                | st^.listTasksFlag     = scrollabletaskWidget st
                | otherwise             = welcomeWidget


welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to Trello-TUI")

helpWidget :: AppState e Name -> Widget Name
helpWidget st = result
  where result | not(st^.dialogFlag) = borderWithLabel (str "Help") $
                                       hLimitPercent 18  $
                                       vLimitPercent 100 $
                                       padBottom Max
                                       (txtWrap $ T.pack $ L.intercalate "\n" helpText)
               | otherwise = emptyWidget

appEvent :: BrickEvent Name e -> EventM Name (AppState e Name) ()
appEvent ev = 
  case ev of
    (VtyEvent (V.EvKey V.KEsc  []))              -> get >>= onExit
    (VtyEvent (V.EvKey V.KEnter  []))            -> get >>= onEnter
    (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) -> get >>= onSave
    (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> get >>= onCancel
    (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) -> get >>= onCreateTask
    (VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) -> filterTasksFlag .= True
    (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl])) -> filterTasksFlag .= False
    (VtyEvent e)                                 -> get >>= onTyping e ev

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
   [  (D.dialogAttr,         V.white `on` V.blue)
    , (D.buttonAttr,         V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    , (E.editAttr,           V.white `on` V.black)
    , (E.editFocusedAttr,    V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]
