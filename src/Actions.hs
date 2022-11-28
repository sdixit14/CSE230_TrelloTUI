module Actions where

import Types
import System.Directory
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Dialog  as D
import Brick.Widgets.Edit as E
import Data.Aeson                  (eitherDecode)
import Data.ByteString.Char8 as B  (pack)
import Data.ByteString.Lazy  as BL (fromStrict)

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


{-
  Upon the event of onExit, update the text file with latest allworkspaces lens
-}

onExit st = do
    -- TODO
    return st

blankAppState :: [Workspace] -> FilePath -> AppState e Name
blankAppState workspaces path = AppState{
                            _workspaces  = workspaces,
                            _workspace   = "",
                            _user        = "",
                            _showDialog  = False,
                            _showEditForm = True,
                            _persistFile = path
                          }

{- 
  Display the Dialog to either 'Create Workspace' or 'Join Workspace'
  If user selects 'Create Workspace' then display form to enter username and workspace name
  If user selects 'Join Workspace' then display form to enter username and workspace name
  Update allworkspaces lens accordingly
-}

displayDialog :: AppState e Name -> Widget Name
displayDialog _ = padLeft (Pad 0) $ padRight Max $ padBottom Max dlgContent
    where dlgContent = D.renderDialog getDialog $ str "What would you like to do?"

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Select one") (Just (0, choices)) 50
    where
        choices = [ ("Create Workspace", Create)
                  , ("Join Workspace", Join)
                  ]

{- 
  Display the Form to create new Task
  Update allworkspaces lens accordingly
-}
displayForm :: AppState e Name -> Widget Name
displayForm _ = padLeft (Pad 0) $ padRight Max $ padBottom Max (E.renderEditor (str . unlines) False (formContent))
    where formContent = E.editor EditField (Just 10) "Start typing your note here" 