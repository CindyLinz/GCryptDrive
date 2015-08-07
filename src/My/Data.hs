module My.Data
  ( Entry(..)
  , EntryVer
  , EntryKey
  , EntryIV
  , FileId
  , latestVer
  , entryToFileStat
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Binary
import Data.Word
import Network.Google.Drive
import Control.Monad
import System.Fuse
import System.Posix.Types

type EntryKey = B.ByteString
type EntryIV = B.ByteString
type EntryVer = Word16

latestVer :: EntryVer
latestVer = 0

-- data EntryType = EntryFile | EntryDir deriving Show
-- instance Binary EntryType where
--   get = do
--     getWord8 >>= \case
--       0 -> return EntryFile
--       1 -> return EntryDir
--       _ -> mzero
-- 
--   put EntryFile = putWord8 0
--   put EntryDir = putWord8 1

data Entry = Entry
  { entryVer :: {-# UNPACK #-} !Word16
  , entryKey :: B.ByteString
  , entryIV :: B.ByteString
  , entryName :: T.Text
  , entryId :: FileId
  , entrySize :: FileOffset
  , entryMode :: FileMode
  , entryCTime :: EpochTime
  , entryMTime :: EpochTime
  } deriving Show

entryToFileStat :: FuseContext -> Entry -> FileStat
entryToFileStat ctx Entry {..} = FileStat
  { statEntryType = fileModeToEntryType entryMode
  , statFileMode = entryMode
  , statLinkCount = case fileModeToEntryType entryMode of { RegularFile -> 1 ; _ -> 2 }
  , statFileOwner = fuseCtxUserID ctx
  , statFileGroup = fuseCtxGroupID ctx
  , statSpecialDeviceID = 0
  , statFileSize = entrySize
  , statBlocks = 1
  , statAccessTime = entryMTime
  , statModificationTime = entryMTime
  , statStatusChangeTime = entryCTime
  }
