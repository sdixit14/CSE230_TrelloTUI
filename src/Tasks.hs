module Tasks where

import           Brick.Focus
import           Brick.Widgets.Border       (border, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (borderStyleFromChar, unicode,
                                             unicodeBold, unicodeRounded)
import           Brick.Widgets.Center       (center, centerAbout)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import           Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)
import           Data.List                  as L (length, map, transpose)
import           Data.List.Split            (chunksOf)
import           Data.Text                  as T hiding (chunksOf, map, unlines)
import           Form
import           Lens.Micro                 (ix, non, (%~), (&), (.~), (^.),
                                             (^?))
import           Lens.Micro.Mtl
import           Brick.Forms
import           Brick.Types
import           Form
import           Types

scrollabletaskWidget :: AppState e Name -> Widget Name
scrollabletaskWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let totalWidth = ctx^.availWidthL
        render $ vLimitPercent 100 $ taskWidgets totalWidth s


getMyTasks st fl = Prelude.filter (\x -> st^.user == x^.assignee)(((fl)!!0)^.tasks)
getMyWorkspace st = Prelude.filter (\x -> (st^.workspace == x^.name)) (st^.workspaces)

taskWidgets :: Int -> AppState e Name -> Widget Name
taskWidgets width st = padLeft (Pad 0) $ padRight Max $ padBottom Max $
  withBorderStyle unicode widgetLayout
    where rows = L.transpose $ splitNotes width 35 val
          filterList = getMyWorkspace st
          val | st^.filterTasksFlag = getMyTasks st filterList
              | otherwise = (((filterList)!!0)^.tasks)
          widgetLayout =  hBox $ L.map (vBox . L.map taskUI) rows
          splitNotes totWidth taskWidth = chunksOf $ totWidth `div` taskWidth

taskUI :: Task -> Widget Name
taskUI t =  withBorderStyle unicodeBold $ hLimit 35 $ vLimit 20 tasklist
  where highlightStyle = withAttr E.editFocusedAttr
        tasklist = borderWithLabel (txt $ t^.title) (padTop (Pad 0) $ highlightStyle $ (txtWrap $ t^.content) <=> (padTop (Pad 2) $ (str "Assigned to:  " <+> (txtWrap $ t^.assignee))))
                  
