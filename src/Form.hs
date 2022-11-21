{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import GHC.Generics
-- import Lens.Micro.Platform
import Lens.Micro.TH (makeLenses)
import Data.Monoid ((<>))

import qualified Graphics.Vty as V
import Brick
import Brick.Main as M (simpleMain)
import Brick.Forms
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

type User = T.Text

data NewTask = NewTask{
    _title :: T.Text,
    _content :: T.Text,
    _assignee :: Username
} deriving (Show)

data Username = User1 | User2 | User3 | User4
            deriving (Show, Eq)
makeLenses ''NewTask

data Name = TitleField
          | ContentField
          | AssigneeField1
          | AssigneeField2
          | AssigneeField3
          | AssigneeField4
          deriving (Eq, Ord, Show)



-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: NewTask -> Form NewTask e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [label "Title" @@=
                    editTextField title TitleField (Just 1),
                label "Content" @@=
                    editTextField content ContentField (Just 3),
                label "Assignee" @@=
                    radioField assignee [ (User1, AssigneeField1, "Sumit"),
                                            (User2, AssigneeField2, "Shikha"),
                                            (User3, AssigneeField3, "Kabir"),
                                            (User4, AssigneeField4, "Phillip")
                                            ]
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form NewTask e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Title must be a text\n" <>
                     "- Content must be a text\n"

app :: App (Form NewTask e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent (V.EvKey V.KEsc []) -> halt
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent f /= Just ContentField -> halt
                _ -> do
                    handleFormEvent ev
                    -- st <- gets formState
                    -- modify $ setFieldValid  AssigneeField

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

ui :: Widget ()
ui = str "Hello World"

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = NewTask { _title = ""
                                   , _content = ""
                                   , _assignee = User1
                                   }
        f = mkForm initialUserInfo
    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f
    
    
    putStrLn "The starting form state was:"
    print initialUserInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
