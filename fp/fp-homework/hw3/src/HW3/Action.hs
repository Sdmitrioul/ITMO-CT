{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DerivingVia #-}

module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Sequence (fromList)
import Data.Set (Set)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (Random (randomR), getStdRandom)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Show, Ord)

data PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

withPermissions :: Foldable t => HiPermission -> IO a -> t HiPermission -> IO a
withPermissions perm doing perms = if perm `elem` perms
  then doing
  else throwIO $ PermissionRequired perm

instance HiMonad HIO where
  runAction = \case
      HiActionCwd               -> HIO (\p -> withPermissions AllowRead (HiValueString . T.pack <$> getCurrentDirectory) p)
      HiActionChDir path        -> HIO (\p -> withPermissions AllowRead (HiValueNull <$ setCurrentDirectory path) p)
      HiActionRead path         -> HIO (\p -> withPermissions AllowRead (readfile path) p)
      HiActionMkDir path        -> HIO (\p -> withPermissions AllowWrite (HiValueNull <$ createDirectory path) p)
      HiActionWrite path output -> HIO (\p -> withPermissions AllowWrite (HiValueNull <$ B.writeFile path output) p)
      HiActionEcho message      -> HIO (\p -> withPermissions AllowWrite (HiValueNull <$ putStrLn  (T.unpack message)) p)
      HiActionRand from to      -> HIO (\_ -> (getStdRandom (randomR (from, to))) >>= return . HiValueNumber . toRational)
      HiActionNow               -> HIO (\p -> withPermissions AllowTime (HiValueTime . systemToUTCTime <$> getSystemTime) p)

readfile :: String -> IO HiValue
readfile file = do
  isFile <- doesFileExist file
  if isFile
  then do
    bytes <- B.readFile file
    case decodeUtf8' bytes of
      Left _    -> return $ HiValueBytes bytes
      Right txt -> return $ HiValueString txt
  else do
    files <- listDirectory file
    return (HiValueList $ fromList $ map (HiValueString . T.pack) files)
