{-# LANGUAGE OverloadedStrings #-}
import Options.Generic

main :: IO ()
main = do
  filePath <- getRecord "face-up"
  putStrLn $ filePath ++ ": unsupported file format"
