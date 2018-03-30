{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Options.Generic

import FaceUp.Video


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-up"
  withVideo filePath (playVideo filePath)
