module My.Test.Common where

import Data.Conduit
import Data.Word
import Data.Bits
import qualified Data.ByteString as B

toHex :: Word8 -> Word8
toHex n
  | n < 10 = 48 + n -- 0~9
  | otherwise = 55 + n -- A~F

encodeHex :: Monad m => Conduit B.ByteString m B.ByteString
encodeHex = await >>= \case
  Just seg -> yield (fst $ B.unfoldrN (B.length seg * 2) encode (Left 0)) >> encodeHex where
    encode (Left i) = Just (toHex (B.index seg i `shiftR` 4), Right i)
    encode (Right i) = Just (toHex (B.index seg i .&. 15), Left (i+1))
  _ -> return ()

