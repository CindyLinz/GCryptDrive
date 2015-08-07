module My.Serialize
  ( encodeDir
  , decodeDir
  , encodeEntry
  , encodeEntry0
  , decodeEntry
  ) where

import My.Data
import My.Exception
import My.Crypt

import Control.Monad.Catch
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Char
import Data.Word
import Data.Conduit
import Data.Binary hiding (encodeFile, decodeFile)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Fuse
import Foreign.C.Types

encodeEntry0 :: Entry -> Put
encodeEntry0 = encodeEntry

encodeEntry :: Entry -> Put
encodeEntry Entry{..} = do
  putWord16le entryVer
  putByteString entryKey
  putByteString entryIV

  putByteString (T.encodeUtf8 entryName)
  putWord8 0

  putByteString (T.encodeUtf8 entryId)
  putWord8 0

  putWord64le (fromIntegral entrySize)
  putWord32le (fromIntegral entryMode)
  putWord64le (let CTime ctime = entryCTime in fromIntegral ctime)
  putWord64le (let CTime mtime = entryMTime in fromIntegral mtime)

decodeEntry :: EntryVer -> Get Entry
decodeEntry parentVer = case parentVer of
  0 -> do
    ver <- getWord16le
    CipherOne emptyCipher <- chooseCipher ver
    let
      keySize = maxCipherKeySize emptyCipher
      ivSize = blockSize emptyCipher

    key <- getByteString keySize
    iv <- getByteString ivSize

    nameBytes <- getLazyByteStringNul
    let name = T.decodeUtf8With T.lenientDecode (BL.toStrict nameBytes)

    fileIdBytes <- getLazyByteStringNul
    let fileId = T.decodeUtf8With T.lenientDecode (BL.toStrict fileIdBytes)

    size <- return . fromIntegral =<< getWord64le
    mode <- return . fromIntegral =<< getWord32le
    ctime <- return . fromIntegral =<< getWord64le
    mtime <- return . fromIntegral =<< getWord64le

    return $ Entry ver key iv name fileId size mode ctime mtime

encodeDir :: (MonadThrow m, MonadIO m) => Entry -> Conduit Entry m B.ByteString
encodeDir entry = go where
  go = await >>= \case
    Nothing -> return ()
    Just entry -> mapM_ yield (BL.toChunks . runPut $ encodeEntry entry) >> go

decodeDir :: (MonadThrow m, MonadIO m) => Entry -> Conduit B.ByteString m Entry
decodeDir entry = go decoder0 where
  decoder0 = runGetIncremental (decodeEntry (entryVer entry))
  go decoder = do
    await >>= \case
      Just seg -> case pushChunk decoder seg of
        Fail _ _ _ -> throwM UnrecognizeFormatException
        Done remain _ entry -> do
          yield entry
          leftover remain
          go decoder0
        next -> go next
      _ -> return ()
