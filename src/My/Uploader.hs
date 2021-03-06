module My.Uploader
  ( spawn
  , done
  , add
  , stats
  ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Exception
import Network.Google.Drive hiding (fileSize)
import qualified Network.Google.Drive as G
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Posix.Files
import System.Posix.IO
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Conduit
import Data.Conduit.List hiding (head, map, take, mapM_, filter)
import Data.Conduit.Binary hiding (head, take, mapM_)
import Data.List (foldl')
import Data.Time.Clock

import My.Data
import My.Crypt
import My.Google

type UploadJob = Entry
data Uploader = Uploader
  { workDir :: FilePath
  , containerFid :: FileId
  , token :: IORef Token
  , uploadEnding :: TVar Bool
  , uploadQueue :: TVar [UploadJob]
  , uploadWorking :: TVar (Maybe UploadJobProgress)
  }

data UploadJobProgress = UploadJobProgress Entry Int {- uploaded size -} deriving Show

tryRemoveLink :: FilePath -> IO Bool
tryRemoveLink path = catch (removeLink path >> return True) (\(e :: SomeException) -> return False)

uploadEntry :: Uploader -> Entry -> IO Bool
uploadEntry Uploader{..} entry = do
  let
    fid = entryId entry
    localPath = workDir </> T.unpack fid

    newFileData :: Maybe Int -> IO G.FileData
    newFileData mSize = do
      return G.FileData
        { fileTitle = "data"
        , fileModified = Nothing
        , fileParents = [containerFid]
        , fileTrashed = False
        , fileSize = mSize
        , fileDownloadUrl = Nothing
        , fileMimeType = "application/octet-stream"
        , fileExportLinks = HM.empty
        }

  CipherOne cipher <- chooseCipher (entryVer entry)

  fstat <- getFileStatus localPath
  let uploadSize = cryptedSize cipher (fromIntegral (fileSize fstat))
  fileData <- newFileData (Just uploadSize)
  putStrLn $ "uploadEntry " ++ show entry ++ " from " ++ localPath ++ " FileData=" ++ show fileData
  putStrLn ""

  lastUpdateRef <- newIORef =<< getCurrentTime

  let
    uploader from = sourceFile localPath $= encryptEntry entry =$= process 0 where
      process offset = do
        liftIO $ atomically $ modifyTVar' uploadWorking $ \(Just (UploadJobProgress entry _)) -> Just (UploadJobProgress entry offset)
        liftIO $ do
          lastUpdate <- readIORef lastUpdateRef
          now <- getCurrentTime
          when (diffUTCTime now lastUpdate > 1) $ do
            putStrLn $ "\ESC[A  " ++ show offset ++ "/" ++ show uploadSize ++ " from " ++ show from
            writeIORef lastUpdateRef now

        await >>= \case
          Nothing -> return ()
          Just chunk -> do
            if offset >= from then do
              yield chunk
            else if offset + B.length chunk > from then
              yield (B.drop (offset + B.length chunk - from) chunk)
            else
              return ()
            process (offset + B.length chunk)

  res <- runGoogleWith token workDir $ do
    G.updateFileWithContent fid fileData uploadSize uploader

  case res of
    Right _ -> do
      tryRemoveLink (localPath <.> "dirty")
      return True
    _ -> return False

run :: Uploader -> IO ()
run uploader@Uploader{..} = go where
  go = do
    atomically takeJob >>= \case
      Nothing -> return ()
      Just job -> do
        doJob job
        go

  takeJob :: STM (Maybe UploadJob)
  takeJob = do
    readTVar uploadQueue >>= \case
      (job:jobs) -> do
        writeTVar uploadQueue jobs
        writeTVar uploadWorking (Just (UploadJobProgress job 0))
        return (Just job)
      [] ->
        readTVar uploadEnding >>= \case
          True -> return Nothing
          False -> retry

  doJob :: UploadJob -> IO ()
  doJob job = go where
    go = do
      isSuccess <- uploadEntry uploader job
      if isSuccess then
        atomically $ writeTVar uploadWorking Nothing
      else
        go

report :: Uploader -> IO ()
report uploader@Uploader{..} = go where
  reportPath = workDir </> "stats"

  --tryCreatePipe =
  --  catch (createNamedPipe reportPath 0o644) (\(e :: SomeException) -> return ())

  go = do
    bracket
      (openFd reportPath WriteOnly (Just 0o644) defaultFileFlags {append = True, trunc = True})
      closeFd
      $ \fd -> do
        uploadHint <- stats uploader
        _ <- fdWrite fd uploadHint
        return ()
    threadDelay 1000000
    go

spawn :: FilePath -> FileId -> IORef Token -> IO Uploader
spawn workDir containerFid token = do
  uploader <- Uploader workDir containerFid token
    <$> newTVarIO False
    <*> newTVarIO []
    <*> newTVarIO Nothing
  _ <- forkIO (run uploader)
  _ <- forkIO (report uploader)
  return uploader

done :: Uploader -> IO ()
done Uploader{..} = do
  atomically $ writeTVar uploadEnding True
  atomically $ do
    readTVar uploadQueue >>= \case
      [] -> readTVar uploadWorking >>= \case
        Nothing -> return ()
        _ -> retry
      _ -> retry

add :: Uploader -> UploadJob -> IO ()
add Uploader{..} job = do
  atomically $ do
    modifyTVar' uploadQueue $ \queue ->
      filter (\e -> entryId e /= entryId job) queue ++ [job]

stats :: Uploader -> IO String
stats Uploader{..} = do
  atomically $ do
    queue <- readTVar uploadQueue
    working <- readTVar uploadWorking

    let
      workingHint = case working of
        Nothing ->
          ""
        Just (UploadJobProgress Entry{..} done) ->
          "uploading " ++ T.unpack entryName ++ " -> " ++ T.unpack entryId ++ " " ++ show done ++ "/" ++ show entrySize ++ "\n"

      pendingSize = foldl' (\acc Entry{..} -> acc + entrySize) 0 queue
      pendingHint = case queue of
        [] -> ""
        _ -> show (length queue) ++ " files pending (" ++ show pendingSize ++ " bytes)\n"

    case working of
      Nothing | null queue -> return "idle.\n"
      _ -> return $ workingHint ++ pendingHint
