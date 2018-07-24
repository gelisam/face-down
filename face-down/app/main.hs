{-# LANGUAGE OverloadedStrings #-}
import Codec.FFmpeg
import Control.Lens
import Options.Generic

import FaceTrace.Stream


main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-down"
  withPictureStream filePath $ \pictureStream -> do
    let scale = 640 / pictureStream ^. streamWidth
        scaledPictureStream = scalePictureStream scale scale pictureStream
    playPictureStream "face-down" scaledPictureStream
