module My.Test.Crypt where

import My.Test.Common

import My.Crypt

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

test :: B.ByteString -> IO ()
test str = do
  let
    CryptoPassed cipher = cipherInit ("12345678901234567890123456789012" :: B.ByteString) :: CryptoFailable AES256
    iv = nullIV

  cipherText <- return . BL.toStrict =<< runConduit (yield str $= encrypt cipher iv $$ sinkLbs)
  cipherTextHex <- return . BL.toStrict =<< runConduit (yield cipherText $= encodeHex $$ sinkLbs)

  B.putStr cipherTextHex
  putStrLn ""

  plainText <- return . BL.toStrict =<< runConduit (yield cipherText $= decrypt cipher iv $$ sinkLbs)

  putStr $ show (B.length plainText) ++ " "
  B.putStr plainText
  putStrLn ""
