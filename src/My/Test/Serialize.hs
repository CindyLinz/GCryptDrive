module My.Test.Serialize where

import My.Test.Common

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Binary
import System.IO
import System.Fuse
import My.Data
import My.Serialize

test :: IO ()
test = do
  let root = Entry 0 "123451234512345123456789067890xx" "abcdefghijklmnop" Directory "name啦啦" "001" 0 0 0 0
  encoded <- runConduit $
    sourceList
      [ Entry 0 "123451234512345123456789067890xx" "abcdefghijklmnop" RegularFile "name啦" "000" 0 0 0 0
      , Entry 0 "123451234512345123456789067890xx" "abcdefghijklmnop" RegularFile "name啦啦" "001" 0 0 0 0
      ] $= encodeDir root $$ sinkLbs

  encodedHex <- runConduit $ sourceLbs encoded $= encodeHex $$ sinkLbs

  BL.putStr encodedHex
  putStrLn ""

  decoded <- runConduit $ sourceLbs encoded $= decodeDir root $$ consume

  putStrLn $ show decoded
