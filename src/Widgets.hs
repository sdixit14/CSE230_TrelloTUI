module Widgets where

import Actions
import Brick.AttrMap
import Brick.Main                 as M
import Brick.Types
import Brick.Util                 (on)
import Brick.Widgets.Border       (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core
import Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)
import Data.List                  as L (elem, intercalate)
import Data.Text                  as T hiding (center, null)
import Graphics.Vty               as V
import Lens.Micro                 (each, (%~), (&), (.~), (^.), (^?))
import Types


helpText = [  "Ctrl-n         : Create Task",
              "Ctrl-s         : Save & close dialog",
              "Ctrl-c         : Cancel Dialog",
              "Esc            : Save State & Exit",
              "F1             : Filter Tasks Assigned to Me"
          ]

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.showDialog = displayDialog st
                -- display all tasks in the given workspace
                | otherwise = welcomeWidget

welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to Trello-TUI")

helpWidget :: AppState e Name -> Widget Name
helpWidget st = result
  where result | not(st^.showDialog) = borderWithLabel (str "Help") $
                                       hLimitPercent 15  $
                                       vLimitPercent 100 $
                                       padBottom Max
                                       (txtWrap $ T.pack $ L.intercalate "\n" helpText)
               | otherwise = emptyWidget

appEvent :: BrickEvent Name e -> EventM Name (AppState e Name) ()
appEvent ev = case ev of
  (VtyEvent (V.EvKey V.KEsc  []))               -> M.halt -- TODO: call onExit and then Halt
  -- (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl]))  -> M.halt
  -- (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))  -> onCancel st
  -- (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]))  -> onSave st >>= M.continueWithoutRedraw
  -- (VtyEvent (V.EvKey (V.KFun 1)  []))           -> onFilter st >>= M.continueWithoutRedraw
  _                                             -> M.continueWithoutRedraw

drawUi :: AppState e Name ->  [Widget Name]
drawUi st = [
      withBorderStyle unicode $
      hLimitPercent 100 $
      vLimitPercent 100 $
      helpWidget st <+>
      borderWithLabel (str "Trello-TUI") (drawLayer st)
    ]

appCursor st = M.showFirstCursor st

app :: M.App (AppState e Name) e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appChooseCursor =  appCursor
          }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ 
    (E.editFocusedAttr, V.white `on` V.black)
  ]
