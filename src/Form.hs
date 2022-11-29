module Form where

import Types
import Brick.Types
import Brick.Forms
import Brick.Widgets.Core
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C


getTaskForm :: Form Task e Name -> Widget Name
getTaskForm f = C.vCenter $ C.hCenter form <=> C.hCenter help
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "Ctrl-s         : Save\n" <>
                     "Ctrl-c         : Cancel\n"

emptyTaskForm = mkTaskForm Task{
  _title   = "",
  _content = ""
}

-- generateListOfTuples :: [User] -> [(User, Name, User)]
-- generateListOfTuples u = map (\x -> (x, AssigneeField, x)) u

mkTaskForm :: Task -> Form Task e Name
mkTaskForm =
    let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [label "Title" @@= editTextField title TitleField (Just 1),
                label "Content" @@= editTextField content ContentField (Just 3)
                -- label "Assigned User" @@= radioField assignee (generateListOfTuples u)
               ]

getWorkspaceForm :: Form FormFields e Name -> Widget Name
getWorkspaceForm f = C.vCenter $ C.hCenter form <=> C.hCenter help
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "Ctrl-s         : Save\n" <>
                     "Ctrl-c         : Cancel\n"

emptyWorkspaceForm = mkWorkspaceForm FormFields{
  _wname    = "",
  _username = ""
}

mkWorkspaceForm :: FormFields -> Form FormFields e Name
mkWorkspaceForm =
    let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [label "Workspace Name" @@= editTextField wname NameField (Just 1),
                label "Username" @@= editTextField username UserField (Just 1)
               ]
