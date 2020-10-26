module Data.Acid.Extra where

import Control.Exception
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import System.Directory
import System.FilePath


load :: (Typeable s, IsAcidic s, SafeCopy s)
     => FilePath -> s -> IO (AcidState s)
load acidStateDir s = openLocalStateFrom acidStateDir s

-- acid-state auto-saves after each update, but that takes up space
consolidate :: IsAcidic s
            => FilePath -> AcidState s -> IO ()
consolidate acidStateDir acidState = do
  let acidStateArchive = acidStateDir </> "Archive"
  createCheckpoint acidState
  createArchive    acidState
  closeAcidState   acidState
  removeDirectoryRecursive acidStateArchive

withAcidState :: (Typeable s, IsAcidic s, SafeCopy s)
              => FilePath -> s -> (AcidState s -> IO a) -> IO a
withAcidState acidStateDir s = bracket (load acidStateDir s)
                                       (consolidate acidStateDir)
