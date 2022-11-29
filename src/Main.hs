module Main where

import Brick.Main as M (defaultMain)
import Control.Monad (void)
import Options.Applicative
import Types
import Actions
import Widgets

main :: IO ()
main =  void $ execParser opts >>= onStart >>= M.defaultMain app

input :: Parser FilePath
input = strOption
          (  long "file"
            <> short 'f'
            <> metavar "FILENAME"
            <> help "File to use for persistece of workspace data for both input/output" )

opts = info (input <**> helper)
        ( fullDesc
          <> progDesc "TUI for Trello"
          <> header "TUI for Trello" )