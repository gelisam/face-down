{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import System.Console.Docopt (Arguments, Docopt, Option)
import System.Environment (getArgs)
import qualified Graphics.Gloss as Gloss
import qualified System.Console.Docopt as Docopt

import GlossActive
import GlossToGif


docopt :: Docopt
docopt = [Docopt.docopt|
Usage:
  play-gif --help
  play-gif <gif_file>

Plays the given file in a loop. ESC to quit.

Options:
  -h, --help    Display this message
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = Docopt.getArgOrExitWith docopt

main :: IO ()
main = do
  args <- Docopt.parseArgsOrExit docopt =<< getArgs
  when (args `Docopt.isPresent` Docopt.longOption "help") $ do
    Docopt.exitWithUsage docopt

  filePath <- args `getArgOrExit` Docopt.argument "gif_file"
  (size, active) <- readGif filePath
  animateActive
    (Gloss.InWindow filePath size size)
    Gloss.black
    active
