module My.Fuse
  ( fuseStart
  , fuseCreate
  ) where

import My.Google
import My.Data
import My.Serialize
import My.Crypt
import My.Exception

import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.Fuse
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.IO
import "unix-bytestring" System.Posix.IO.ByteString
-- import System.Posix.Directory
import System.Posix.Time
import System.IO
import System.IO.Error
import Foreign.C.Error
import qualified Data.HashTable.IO as H
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.List hiding (head, map, take, mapM_)
import Data.Conduit.Binary hiding (head, take, mapM_)
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Network.Google.Drive as G
import qualified Network.Google.Drive.Upload as G

type HashTable k v = H.BasicHashTable k v

instance Show OpenMode where
  show ReadOnly = "ReadOnly"
  show WriteOnly = "WriteOnly"
  show ReadWrite = "ReadWrite"

instance Show OpenFileFlags where
  show (OpenFileFlags {..}) =
    map (\case True -> '1' ; False -> '0')
      [append, exclusive, noctty, nonBlock, trunc]

removeGoogleEntry :: IORef Token -> FilePath -> FileId -> IO ()
removeGoogleEntry token workDir fid = do
  putStrLn $ "removeGoogleEntry " ++ T.unpack fid
  runGoogleWith token workDir (G.deleteFile G.File {fileId = fid, fileData = undefined}) >>= \case
    Left _ -> throwM eFAULT
    Right _ -> return ()

createGoogleEntry :: IORef Token -> FilePath -> FileId -> T.Text -> EntryType -> IO Entry
createGoogleEntry token workDir containerFid name ty = do
  putStrLn $ "createGoogleEntry " ++ T.unpack name ++ " " ++ show ty

  res <- runGoogleWith token workDir $ do
    G.createFile G.FileData
      { fileTitle = "data"
      , fileModified = Nothing
      , fileParents = [containerFid]
      , fileTrashed = False
      , fileSize = Nothing
      , fileDownloadUrl = Nothing
      , fileMimeType = "application/octet-stream"
      , fileExportLinks = HM.empty
      }

  case res of
    Left e -> throwM $ mkIOError
      userErrorType
      ("Failed to create a file under Google directory " ++ T.unpack containerFid ++ ": " ++ show e)
      Nothing Nothing
    Right file -> do
      CipherOne cipher <- chooseCipher latestVer
      newKey <- randomKey cipher
      newIV <- randomIV cipher
      now <- epochTime
      return Entry
        { entryVer = latestVer
        , entryKey = newKey
        , entryIV = newIV
        , entryName = name
        , entryId = G.fileId file
        , entrySize = 0
        , entryMode = unionFileModes (case ty of { Directory -> 0o755 ; _ -> 0o644 }) (entryTypeToFileMode ty)
        , entryCTime = now
        , entryMTime = now
        }

tryRemoveLink :: FilePath -> IO Bool
tryRemoveLink path = catch (removeLink path >> return True) (\(e :: SomeException) -> return False)

fuseCreate :: FilePath -> FileId -> IO ()
fuseCreate workDir containerFid = do
  createDirectoryIfMissing True workDir
  token <- getGoogleToken workDir >>= newIORef
  rootEntry <- createGoogleEntry token workDir containerFid "" Directory
  BL.writeFile (workDir </> ".root") (runPut (encodeEntry0 rootEntry))

fuseStart :: FilePath -> FilePath -> Bool -> IO ()
fuseStart mountDir workDirRel isDebug = do
  workDir <- makeAbsolute workDirRel
  rootEntry <- BL.readFile (workDir </> ".root") >>= return . runGet (decodeEntry 0)
  let rootId = entryId rootEntry
  token <- getGoogleToken workDir >>= newIORef
  runGoogleWith token workDir (G.getFile rootId) >>= \case
    Left err -> do
      putStrLn $ "Failed to fetch root file on the Google Drive by root id: " ++ T.unpack rootId ++ " " ++ show err
    Right Nothing ->
      putStrLn $ "Can't find root file on the Google Drive by root id: " ++ T.unpack rootId
    Right (Just root) -> do
      let containerFid = head (G.fileParents (G.fileData root) ++ ["root"])

      pathIndex <- H.new :: IO (HashTable FilePath Entry)
      H.insert pathIndex "/" rootEntry

      let
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

        updateDir :: FilePath -> (Maybe Entry -> ConduitM Entry Entry (ResourceT IO) [Entry]) -> IO ()
        updateDir path modifyEntry = do
          locate path >>= \case
            Nothing -> throwM eNOENT
            Just entry -> do
              source <- dirSource entry
              sink <- dirSink entry
              let
                process = do
                  next <- await
                  modifyEntry next >>= mapM_ yield
                  case next of
                    Just _ -> process
                    Nothing -> return ()
              runResourceT $ source $$ process =$ sink

        uploadEntry :: Entry -> IO ()
        uploadEntry entry = do
          let
            fid = entryId entry
            localPath = workDir </> T.unpack fid

          CipherOne cipher <- chooseCipher (entryVer entry)

          fstat <- getFileStatus localPath
          let uploadSize = cryptedSize cipher (fromIntegral (fileSize fstat))
          fileData <- newFileData (Just uploadSize)
          putStrLn $ "uploadEntry " ++ show entry ++ " from " ++ localPath ++ " FileData=" ++ show fileData

          let
            uploader from = sourceFile localPath $= encryptEntry entry =$= process 0 where
              process offset = do
                liftIO $ putStrLn $ "  " ++ show offset ++ "/" ++ show uploadSize ++ " from " ++ show from
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
              return ()
            _ -> throwM eBUSY

        downloadEntry :: Entry -> IO ()
        downloadEntry entry = do
          putStrLn $ "downloadEntry " ++ show entry
          let
            fid = entryId entry
            localPath = workDir </> T.unpack fid
            localDownloadPath = localPath <.> "download"

          putStrLn $ "localDownloadPath = " ++ localDownloadPath

          res <- runGoogleWith token workDir $ do
            G.getFile fid >>= \case
              Nothing -> return Nothing
              Just file -> do
                -- liftIO $ putStrLn $ "file = " ++ show file
                G.downloadFile file $ \s -> do
                  -- liftIO $ putStrLn $ "got source"
                  s $$+- decryptEntry entry =$ sinkFile localDownloadPath

          putStrLn $ "res = " ++ show res

          case res of
            Right (Just _) -> do
              rename localDownloadPath localPath
            _ -> do
              throwM (mkIOError doesNotExistErrorType "downloadEntry" Nothing (Just localPath))

        makeSureEntry :: Entry -> IO ()
        makeSureEntry entry = do
          let
            fid = entryId entry
            localPath = workDir </> T.unpack fid

          isExist <- fileExist localPath
          when (not isExist) (downloadEntry entry)

        dirSource :: (MonadResource m, MonadThrow m) => Entry -> IO (Source m Entry)
        dirSource entry = do
          case fileModeToEntryType (entryMode entry) of
            Directory -> return ()
            _ -> throwM eNOTDIR

          res <- makeSureEntry entry
          let
            fid = entryId entry
            localPath = workDir </> T.unpack fid
          return $ sourceFile localPath $= decodeDir entry

        dirSink :: (MonadIO m, MonadResource m, MonadThrow m) => Entry -> IO (Sink Entry m ())
        dirSink entry = do
          case fileModeToEntryType (entryMode entry) of
            Directory -> return ()
            _ -> throwM eNOTDIR

          let
            fid = entryId entry
            localPath = workDir </> T.unpack fid
            localEditPath = workDir </> T.unpack fid <.> "edit"

          tryRemoveLink localEditPath

          let
            after True = liftIO $ do
              tryRemoveLink localPath
              rename localEditPath localPath
              uploadEntry entry
            after False = liftIO $ tryRemoveLink localEditPath >> return ()

          return (encodeDir entry =$= addCleanup after (sinkFile localEditPath))

--         entrySource :: Entry -> IO (Source IO B.ByteString)
--         entrySource entry = do
--           let
--             fid = entryId entry
--             localPath = workDir </> T.unpack fid
-- 
--           isExist <- fileExists localPath
--           when (not isExist) $ do
--             downloadEntry entry
--           return $ sourceFile localPath

        locate :: FilePath -> IO (Maybe Entry)
        locate path = do
          putStrLn $ "locate " ++ path
          H.lookup pathIndex path >>= \case
            Just entry -> return (Just entry)
            Nothing -> do
              let
                fileName = T.pack (takeFileName path)
                dirPath = takeDirectory path
              locate dirPath >>= \case
                Nothing -> return Nothing
                Just parentEntry -> do
                  source <- dirSource parentEntry
                  runResourceT $ source $$ do
                    let
                      go :: (MonadThrow m, MonadResource m) => Sink Entry m (Maybe Entry)
                      go = do
                        await >>= \case
                          Nothing -> return Nothing
                          Just entry -> do
                            if entryName entry == fileName then do
                              return (Just entry)
                            else
                              go
                    go

        fuseOps = defaultFuseOps
          { fuseInit = putStrLn "fuseInit"

          , fuseDestroy = putStrLn "fuseDestroy"

          , fuseCreateDevice = \path ty mode devId -> catchPosix_ $ do
            putStrLn $ "fuseCreateDevice " ++ path ++ " " ++ show ty ++ " " ++ show mode ++ " " ++ show devId

            case ty of
              RegularFile -> return ()
              _ -> throwM eFAULT

            locate path >>= \case
              Just _ -> throwM eEXIST
              Nothing -> return ()

            let
              parentPath = takeDirectory path
              name = T.pack $ takeFileName path

            updateDir parentPath $ \case
              Just entry
                | entryName entry == name -> throwM eEXIST
                | otherwise -> return [entry]
              Nothing -> do
                newEntry <- liftIO $ createGoogleEntry token workDir containerFid name ty
                return [newEntry]

          , fuseOpen = \path mode flags -> do
            putStrLn $ "fuseOpen " ++ path ++ " " ++ show mode ++ " " ++ show flags
            locate path >>= \case
              Nothing -> return (Left eNOENT)
              Just entry -> do
                makeSureEntry entry
                fd <- openFd (workDir </> T.unpack (entryId entry)) mode (Just 0o644) flags
                return (Right (entry, fd))

          , fuseRead = \path (entry, fd) limit offset -> catchPosix $ do
            putStrLn $ "fuseRead " ++ path ++ " " ++ show offset ++ " " ++ show limit
            let
              process = do
                res <- fdPread fd limit offset
                -- putStrLn $ "  res = " ++ show res
                putStrLn $ "  res."
                return res
            catch process (\e -> if isEOFError e then return B.empty else ioError e)

          , fuseWrite = \path (entry, fd) dat offset -> catchPosix $ do
            putStrLn $ "fuseWrite " ++ path ++ " " ++ show offset ++ " " ++ show (B.length dat)
            -- putStrLn $ "fuseWrite " ++ path ++ " " ++ show offset ++ " " ++ show dat

            let localPath = workDir </> T.unpack (entryId entry)
            when (B.length dat > 0) $ do
              catch (createDevice (localPath <.> "dirty") regularFileMode 0) (\(e :: SomeException) -> return ())

            fdPwrite fd dat offset

          , fuseSetFileSize = \path size -> catchPosix_ $ do
            putStrLn $ "fuseSetFileSize " ++ path ++ " " ++ show size
            locate path >>= \case
              Just entry @ Entry {entryMode = (fileModeToEntryType -> RegularFile), entryId = fid} -> do
                setFileSize (workDir </> T.unpack fid) size
                let entry' = entry { entrySize = size }
                H.insert pathIndex path entry'
                uploadEntry entry'

                updateDir (takeDirectory path) $ \case
                  Nothing -> return []
                  Just entry
                    | entryId entry == fid -> return [entry']
                    | otherwise -> return [entry]
              _ ->
                throwM eNOENT

          , fuseSetFileMode = \path mode -> catchPosix_ $ do
            putStrLn $ "fuseSetFileMode " ++ path ++ " " ++ show mode
            locate path >>= \case
              Nothing -> throwM eNOENT
              Just entry -> do
                let entry' = entry {entryMode = mode}

                updateDir (takeDirectory path) $ \case
                  Nothing -> return []
                  Just entry
                    | entryId entry == entryId entry' -> return [entry']
                    | otherwise -> return [entry]

          , fuseSetFileTimes = \path _ mtime -> catchPosix_ $ do
            putStrLn $ "fuseSetFileTimes " ++ path ++ " " ++ show mtime
            locate path >>= \case
              Nothing -> throwM eNOENT
              Just entry -> do
                let entry' = entry {entryMTime = mtime}

                updateDir (takeDirectory path) $ \case
                  Nothing -> return []
                  Just entry
                    | entryId entry == entryId entry' -> return [entry']
                    | otherwise -> return [entry]

                H.insert pathIndex path entry'

          , fuseSetOwnerAndGroup = \path uid gid -> return eOK

          , fuseFlush = \path (entry, fd) -> do
            putStrLn $ "fuseFlush " ++ path

            let localPath = workDir </> T.unpack (entryId entry)
            fileExist (localPath <.> "dirty") >>= \case
              False -> do
                putStrLn "  skipped."
                return eOK

              True -> do
                fstat <- getFileStatus localPath
                now <- epochTime
                let entry' = entry { entrySize = fileSize fstat, entryMTime = now }
                H.insert pathIndex path entry'
                uploadEntry entry'

                updateDir (takeDirectory path) $ \case
                  Nothing -> return []
                  Just entry
                    | entryId entry == entryId entry' -> return [entry']
                    | otherwise -> return [entry]

                return eOK

          , fuseRelease = \path (entry, fd) -> do
            putStrLn $ "fuseRelease " ++ path
            -- uploadEntry entry
            closeFd fd
            return ()

          , fuseRemoveLink = \path -> catchPosix_ $ do
            putStrLn $ "fuseRemoveLink " ++ path
            locate path >>= \case
              Nothing -> throwM eNOENT
              Just entry -> do
                let parentPath = takeDirectory path
                updateDir parentPath $ \case
                  Just entry'
                    | entryId entry' == entryId entry -> return []
                    | otherwise -> return [entry']
                  Nothing -> do
                    return []

                let localPath = workDir </> T.unpack (entryId entry)
                tryRemoveLink localPath
                H.delete pathIndex path

                removeGoogleEntry token workDir (entryId entry)

          , fuseGetFileStat = \path -> do
            putStrLn $ "fuseGetFileStat " ++ path
            ctx <- getFuseContext
            locate path >>= \case
              Nothing ->
                return $ Left eNOENT
              Just entry -> do
                let stat = entryToFileStat ctx entry
                putStrLn $ "  stat = " ++ show stat
                return . Right $ stat

          --, fuseGetFileSystemStats = undefined
          , fuseOpenDirectory = \path -> do
            putStrLn $ "fuseOpenDirectory " ++ path
            locate path >>= \case
              Nothing -> return eNOENT
              Just entry ->
                case fileModeToEntryType (entryMode entry) of
                  Directory -> return eOK
                  _ -> return eNOENT

          , fuseReadDirectory = \path -> do
            putStrLn $ "fuseReadDirectory " ++ path
            ctx <- getFuseContext
            locate path >>= \case
              Nothing -> return (Left eNOENT)
              Just entry -> do
                Just parentEntry <- locate (takeDirectory path)
                case fileModeToEntryType (entryMode entry) of
                  Directory -> do
                    source <- dirSource entry

                    let
--                       pathTxt = T.pack path
--                       validLen = findEnd (T.length pathTxt) where
--                         findEnd len
--                           | len == 0 || T.index pathTxt (len - 1) /= pathSeparator = len
--                           | otherwise = findEnd (len - 1)
--                       validPath = take validLen path

                      processEntry :: MonadIO m => Conduit Entry m (FilePath, FileStat)
                      processEntry = do
                        yield (".", entryToFileStat ctx entry)
                        yield ("..", entryToFileStat ctx parentEntry)
                        go
                        where
                          go = await >>= \case
                            Nothing -> return ()
                            Just entry -> do
                              let name = T.unpack $ entryName entry
                              liftIO (H.insert pathIndex (path </> name) entry)
                              yield (name, entryToFileStat ctx entry)
                              go

                    res <- runResourceT $ source $$ processEntry =$ consume
                    return (Right res)
                  _ -> return (Left eNOTDIR)

          , fuseCreateDirectory = \path mode -> catchPosix_ $ do
            putStrLn $ "fuseCreateDirectory " ++ path ++ " " ++ show mode
            locate path >>= \case
              Just _ -> throwM eEXIST
              Nothing -> return ()

            let
              parentPath = takeDirectory path
              name = T.pack $ takeFileName path

            updateDir parentPath $ \case
              Just entry
                | entryName entry == name -> throwM eEXIST
                | otherwise -> return [entry]
              Nothing -> do
                newEntry <- liftIO $ createGoogleEntry token workDir containerFid name Directory
                return [newEntry]

          , fuseRemoveDirectory = \path -> catchPosix_ $ do
            putStrLn $ "fuseRemoveDirectory " ++ path
            locate path >>= \case
              Nothing -> throwM eNOENT
              Just entry -> do
                source <- dirSource entry
                isEmpty <- runResourceT $ source $$ do
                  await >>= \case
                    Nothing -> return True
                    Just _ -> return False
                case isEmpty of
                  False -> throwM eNOTEMPTY
                  True -> do
                    tryRemoveLink (workDir </> T.unpack (entryId entry))
                    H.delete pathIndex path
                    updateDir (takeDirectory path) $ \case
                      Just entry'
                        | entryId entry' == entryId entry -> return []
                        | otherwise -> return [entry']
                      Nothing ->
                        return []

                    removeGoogleEntry token workDir (entryId entry)
          }

      prog <- getProgName
      fuseRun prog (mountDir : (if isDebug then ["-d"] else [])) fuseOps defaultExceptionHandler
      -- fuseRun prog [mountDir] fuseOps defaultExceptionHandler

