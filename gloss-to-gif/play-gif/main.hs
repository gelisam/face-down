{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.Console.Docopt (Arguments, Docopt, Option)
import System.Environment (getArgs)
import qualified Graphics.Gloss as Gloss
import qualified System.Console.Docopt as Docopt

import GlossActive
import GlossToGif


patterns :: Docopt
patterns = [Docopt.docopt|
Usage:
  play-gif <gif_file>

Plays the given file in a loop. ESC to quit.
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = Docopt.getArgOrExitWith patterns

main :: IO ()
main = do
  args <- Docopt.parseArgsOrExit patterns =<< getArgs
  filePath <- args `getArgOrExit` Docopt.argument "gif_file"
  (size, active) <- readGif filePath
  animateActive
    (Gloss.InWindow filePath size size)
    Gloss.black
    active
