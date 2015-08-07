{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import My.Data
-- import My.Crypt
-- import My.Google
-- import My.Serialize
import Network.Google.Drive
import System.FilePath
import My.Fuse

-- import My.Test.Crypt
-- import My.Test.Serialize

--import Control.Exception
import Control.Monad.Catch
import System.Environment
import System.Console.GetOpt
import Data.List
import Data.Typeable
import qualified Data.Text as T

data Opt = Opt
  { optCreate :: Maybe FileId
  , optMount :: Maybe FilePath
  , optWork :: Maybe FilePath
  , optDebug :: Bool
  }

optsConfig :: [OptDescr (Opt -> Opt)]
optsConfig =
  [ Option ['c'] ["create"] (ReqArg (\containerId opt -> opt {optCreate = Just $ T.pack containerId}) "GDrive folder id") "Create a new GCryptDrive volumn under the specified GDrive folder, and stored the entry point in the work directory"
  , Option ['m'] ["mount"] (ReqArg (\mntPath opt -> opt {optMount = Just mntPath}) "mount point") "Mount a GCryptDrive volumn, which is specified in the work directory, to the mount point"
  , Option ['w'] ["work_dir"] (ReqArg (\workPath opt -> opt {optWork = Just workPath}) "local work directory") ""
  , Option ['d'] ["debug"] (NoArg (\opt -> opt {optDebug = True})) "debug mode"
  ]

data WrongArgumentsException = WrongArgumentsException String
  deriving (Show, Typeable)
instance Exception WrongArgumentsException

main = catch go $ \(WrongArgumentsException msg) -> do
    putStrLn msg
    prog <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ prog ++ " [options]") optsConfig
  where
    go = do
      args <- getArgs
      let
        (parsedOpts, _, errs) = getOpt Permute optsConfig args
        Opt {..} = foldl (\o f -> f o) (Opt Nothing Nothing Nothing False) parsedOpts

      case optWork of
        Nothing -> throwM $ WrongArgumentsException "work_dir is required"
        Just workDir -> case optMount of
          Just mntPath -> do
            fuseStart mntPath workDir optDebug
          _ -> case optCreate of
            Just containerId -> do
              fuseCreate workDir containerId
            _ -> throwM $ WrongArgumentsException "You must create a volumn or mount a volumn"

--   runGoogle_ $ do
--     Just root <- getFile "1OnCAeMCnLwJ8knzQeQw8BgTuYjHXSXpIS1JJ6uGzzXk"
--     items <- listVisibleContents root
--     liftIO $ putStrLn $ show items
