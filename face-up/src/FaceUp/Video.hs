module FaceUp.Video where

import Codec.FFmpeg
import Codec.Picture
import Control.Exception
import Control.Lens
import Data.Maybe
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Juicy


data Video = Video
  { videoNextFrameTime :: IO (Maybe (Image PixelRGB8, Double))
  , videoCleanup       :: IO ()
  }

videoNextFrame :: Video -> IO (Maybe (Image PixelRGB8))
videoNextFrame video = over _Just fst <$> videoNextFrameTime video

withVideo :: FilePath
          -> (Video -> IO a)
          -> IO a
withVideo filePath = bracket acquire release
  where
    acquire :: IO Video
    acquire = fmap (uncurry Video)
            . imageReaderTime
            . File
            $ filePath

    release :: Video -> IO ()
    release = videoCleanup


defaultImage :: Image PixelRGB8
defaultImage = generateImage (\_ _ -> PixelRGB8 0 0 0) 640 480

playVideo :: String -> Video -> IO ()
playVideo windowTitle video = do
  firstFrame <- fromMaybe defaultImage <$> videoNextFrame video

  let nextFrame :: Float -> IO Picture
      nextFrame _ = maybe mempty fromImageRGB8 <$> videoNextFrame video

  animateFixedIO (InWindow windowTitle (imageWidth firstFrame, imageHeight firstFrame) (10, 10))
                 black
                 nextFrame
                 mempty
