{-# LANGUAGE QuasiQuotes #-}
module Main where

import Options.Applicative ((<**>))
import qualified Graphics.Gloss as Gloss
import qualified Options.Applicative as Opt

import GlossActive
import GlossToGif


cliArgs :: Opt.Parser FilePath
cliArgs
  = Opt.argument Opt.str (Opt.metavar "FILE.gif")

cliHelp :: Opt.InfoMod a
cliHelp
  = Opt.fullDesc
 <> Opt.progDesc "Plays the given file in a loop. ESC to quit."

cliApi :: Opt.ParserInfo FilePath
cliApi
  = Opt.info (cliArgs <**> Opt.helper) cliHelp

main :: IO ()
main = do
  filePath <- Opt.execParser cliApi
  (size, active) <- readGif filePath
  animateActive
    (Gloss.InWindow filePath size size)
    Gloss.black
    active
