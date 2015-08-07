module My.Crypt
  ( encrypt
  , decrypt
  , encryptEntry
  , decryptEntry
  , maxCipherKeySize
  , randomKey
  , randomIV
  , cryptedSize
  , chooseCipher
  , prepareCipher
  , blockSize
  , CipherOne (..)
  , CipherPair (..)
  ) where

import My.Data
import My.Exception

import Data.Conduit
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Random
import Crypto.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data CipherOne = forall cipher. BlockCipher cipher => CipherOne cipher
data CipherPair = forall cipher. BlockCipher cipher => CipherPair cipher (IV cipher)

chooseCipher :: MonadPlus m => EntryVer -> m CipherOne
chooseCipher 0 = return $ CipherOne (undefined :: AES256)
chooseCipher _ = mzero

prepareCipher :: MonadPlus m => Entry -> m CipherPair
prepareCipher Entry{ entryVer=0, entryKey=key, entryIV=ivStr } = case makeIV ivStr of
  Just iv -> return (CipherPair cipher iv) where
    CryptoPassed cipher = cipherInit key :: CryptoFailable AES256
  _ -> mzero
prepareCipher _ = mzero

randomKey :: BlockCipher cipher => cipher -> IO B.ByteString
randomKey cipher = do
  drg <- getSystemDRG
  return $ fst $ randomBytesGenerate (maxCipherKeySize cipher) drg

randomIV :: BlockCipher cipher => cipher -> IO B.ByteString
randomIV cipher = do
  drg <- getSystemDRG
  return $ fst $ randomBytesGenerate (blockSize cipher) drg

cryptedSize :: BlockCipher cipher => cipher -> Int -> Int
cryptedSize cipher plainSize =
  chunkSize * (plainSize `div` chunkSize + 1)
  where
    chunkSize = blockSize cipher

maxCipherKeySize :: Cipher cipher => cipher -> Int
maxCipherKeySize cipher = case cipherKeySize cipher of
  KeySizeRange _ s -> s
  KeySizeEnum ss -> foldr max 0 ss
  KeySizeFixed s -> s

-- the size of the last chunk is in [0,chunkSize)
byChunk :: MonadIO m => Int -> Conduit B.ByteString m B.ByteString
byChunk size = go 0 [] where
  go len pendings = do
    -- liftIO $ putStrLn $ "byChunk go " ++ show len ++ " pendings=" ++ show pendings
    await >>= \case
      Nothing -> yield $ BL.toStrict . BL.fromChunks $ reverse pendings
      Just fragment
        | len' < size -> go len' (fragment : pendings)
        | otherwise -> do
          yield $ BL.toStrict . BL.fromChunks $ reverse (formerFrag : pendings)
          go2 laterFrag
        where
          len' = len + B.length fragment
          (formerFrag, laterFrag) = B.splitAt (size - len) fragment
          go2 frag
            | B.length frag >= size = do
              let (former, later) = B.splitAt size frag
              yield former
              go2 later
            | otherwise = go (B.length frag) [frag]

-- cryptMainLoop :: Monad m => Int -> a -> (a -> B.ByteString -> (a, B.ByteString)) -> Conduit B.ByteString m B.ByteString
-- cryptMainLoop size state coreAction =
--   let
--     process iv len segs
--       | len >= size =
--         let
--           (former, later) = BL.splitAt (fromIntegral size) $ BL.fromChunks $ reverse segs
--           (state', cipherText) = coreAction state (BL.toStrict former)
--         in do
--           yield cipherText
--           if len == size then
--             process state' 0 []
--           else
--             process state' (len - size) (reverse $ BL.toChunks later)
-- 
--       | otherwise = fetch state len segs
-- 
--     fetch state len segs = do
--       await >>= \case
--         Just seg -> process state (len + B.length seg) (seg : segs)
--         _ -> if null segs
--           then return ()
--           else process state size (B.replicate (size - len) 35 : segs)
--   in
--     fetch state 0 []

encryptEntry :: (MonadThrow m, MonadIO m) => Entry -> Conduit B.ByteString m B.ByteString
encryptEntry entry = do
  case prepareCipher entry of
    Just (CipherPair cipher iv) -> encrypt cipher iv
    _ -> throwM UnrecognizeFormatException

decryptEntry :: (MonadThrow m, MonadIO m) => Entry -> Conduit B.ByteString m B.ByteString
decryptEntry entry = do
  case prepareCipher entry of
    Just (CipherPair cipher iv) -> decrypt cipher iv
    _ -> throwM UnrecognizeFormatException

encrypt :: (BlockCipher cipher, MonadIO m) => cipher -> IV cipher -> Conduit B.ByteString m B.ByteString
encrypt cipher iv = byChunk chunkSize =$= process where
  chunkSize = blockSize cipher
  process = go iv where
    go iv = do
      Just chunk <- await
      let chunkLen = B.length chunk

      if chunkLen < chunkSize then
        yield $ cfbEncrypt cipher iv $ BL.toStrict $ BL.fromChunks
          [ chunk
          , B.replicate (fromIntegral $ chunkSize - chunkLen - 1) 0
          , B.singleton (fromIntegral chunkLen)
          ]
      else do
        let
          cipherText = cfbEncrypt cipher iv chunk
          Just iv' = makeIV cipherText
        yield cipherText
        go iv'
--   cryptMainLoop (blockSize cipher) (cipher, iv) encryptCore where
--     encryptCore (cipher, iv) plainText = ((cipher, iv'), cipherText) where
--       cipherText = cfbEncrypt cipher iv plainText
--       Just iv' = makeIV cipherText

decrypt :: (BlockCipher cipher, MonadIO m) => cipher -> IV cipher -> Conduit B.ByteString m B.ByteString
decrypt cipher iv = byChunk chunkSize =$= process where
  chunkSize = blockSize cipher
  process = do
    await >>= \case
      Nothing -> return ()
      Just chunk
        | B.length chunk < chunkSize -> return ()
        | otherwise -> go iv chunk
      where
        go iv lastChunk = do
          let plainText = cfbDecrypt cipher iv lastChunk
          Just chunk <- await

          if B.length chunk == chunkSize then do
            yield plainText
            let Just iv' = makeIV lastChunk
            go iv' chunk
          else do
            let len = fromIntegral $ B.index plainText (chunkSize - 1)
            when (len > 0) (yield $ B.take len plainText)
--   cryptMainLoop (blockSize cipher) (cipher, iv) decryptCore where
--     decryptCore (cipher, iv) cipherText = ((cipher, iv'), plainText) where
--       plainText = cfbDecrypt cipher iv cipherText
--       Just iv' = makeIV cipherText
