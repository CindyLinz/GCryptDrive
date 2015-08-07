module My.Exception
  ( UnrecognizeFormatException(..)
  , catchPosix
  , catchPosix_
  ) where

import Control.Exception
import Data.Functor
import Data.Typeable
import Foreign.C.Error

data UnrecognizeFormatException = UnrecognizeFormatException
  deriving (Show, Typeable)
instance Exception UnrecognizeFormatException

instance Show Errno where
  show (Errno errno) = show errno

deriving instance Typeable Errno
instance Exception Errno

catchPosix :: IO a -> IO (Either Errno a)
catchPosix action = catch (fmap Right action) $ \e -> return (Left (e :: Errno))

catchPosix_ :: IO () -> IO Errno
catchPosix_ action = catch (action >> return eOK) $ \e -> return (e :: Errno)
