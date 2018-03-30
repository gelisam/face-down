{-# LANGUAGE LambdaCase, OverloadedStrings, TypeApplications #-}
import Codec.FFmpeg
import Codec.Picture
import Control.Exception
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Options.Generic

data Video = Video
  { videoNextFrame :: IO (Maybe (Image PixelRGB8))
  , videoCleanup   :: IO ()
  }

withVideo :: FilePath
          -> (Video -> IO a)
          -> IO a
withVideo filePath = bracket acquire release
  where
    acquire :: IO Video
    acquire = fmap (uncurry Video)
            . imageReader
            . File
            $ filePath

    release :: Video -> IO ()
    release = videoCleanup

main :: IO ()
main = do
  initFFmpeg
  filePath <- getRecord "face-up"
  withVideo filePath $ \video -> do
    videoNextFrame video >>= \case
      Nothing -> putStrLn "empty video."
      Just image -> do
        let picture = fromImageRGB8 image
        display (InWindow "Nice Window" (imageWidth image, imageHeight image) (10, 10))
                white
                picture
