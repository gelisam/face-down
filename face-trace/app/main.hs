{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Options.Generic

import FaceTrace


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-trace"
  runApp "face-trace" filePath
