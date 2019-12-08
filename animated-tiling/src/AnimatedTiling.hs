module AnimatedTiling where

writeGif
  :: FilePath
  -> IO ()
writeGif filePath = do
  writeFile filePath ""
