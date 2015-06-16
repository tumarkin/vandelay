module Paths_vandelay (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/roberttumarkin/Library/Haskell/bin"
libdir     = "/Users/roberttumarkin/Library/Haskell/ghc-7.8.3-x86_64/lib/vandelay-0.1.0.0"
datadir    = "/Users/roberttumarkin/Library/Haskell/share/ghc-7.8.3-x86_64/vandelay-0.1.0.0"
libexecdir = "/Users/roberttumarkin/Library/Haskell/libexec"
sysconfdir = "/Users/roberttumarkin/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vandelay_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vandelay_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "vandelay_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vandelay_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vandelay_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
