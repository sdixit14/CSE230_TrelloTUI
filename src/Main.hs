module Main where

import Brick as B
import Brick.Main as M (simpleMain)
import Control.Monad (void)
import Options.Applicative
import Types

ui :: B.Widget ()
ui = B.str "Hello World"

main :: IO ()
main =  void $ M.simpleMain ui

input :: Parser CmdInputOptions
input = FileInput 
        <$> strOption
          (  long "file"
            <> short 'f'
            <> metavar "FILENAME"
            <> help "File to use for persistece of workspace data for both input/output" )

opts = info (input <**> helper)
        ( fullDesc
          <> progDesc "TUI for Trello"
          <> header "TUI for Trello" )