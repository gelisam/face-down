{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Options.Generic

import FaceTrace.Video
import FaceTrace.VideoPlayer


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-trace"
  withVideo filePath (videoPlayer filePath)
