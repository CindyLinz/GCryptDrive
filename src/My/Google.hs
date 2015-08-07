module My.Google
  ( runGoogle
  , runGoogleWith
  , getGoogleToken
  , Token
  ) where

import Network.Google.OAuth2
import Network.Google.Drive

import Data.IORef
import System.FilePath

type Token = String

deriving instance Show FileData

getGoogleToken :: FilePath -> IO Token
getGoogleToken workDir = do
  let
    client = OAuth2Client
      "419521340217-v7k30njgu1s2a872489cp30hl6nu247l.apps.googleusercontent.com"
      "LfdjyTBv11auvSEOAr7A2UoQ"
    scopes = ["https://www.googleapis.com/auth/drive"]
  getAccessToken client scopes (Just (workDir </> ".token"))

runGoogle :: FilePath -> Api a -> IO (Either ApiError a)
runGoogle workDir action = do
  token <- getGoogleToken workDir >>= newIORef
  runGoogleWith token workDir action

runGoogleWith :: IORef Token -> FilePath -> Api a -> IO (Either ApiError a)
runGoogleWith rToken workDir act = go 0 where
  go retried = do
    token <- readIORef rToken
    runApi token act >>= \case
      Left e
        | retried >= 3 -> return (Left e)
        | otherwise -> do
          putStrLn $ "got error and retrying: " ++ show e
          token' <- getGoogleToken workDir
          writeIORef rToken token'
          go (retried + 1)
      Right r -> return (Right r)

