{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Options.Generic

import FaceTrace.VideoPlayer


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-trace"
  videoPlayer "face-trace" filePath
