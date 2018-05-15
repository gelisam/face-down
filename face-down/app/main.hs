{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Options.Generic

import FaceTrace.PictureStream


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-down"
  withPictureStream filePath $ \pictureStream -> do
    playPictureStream "face-down" pictureStream
