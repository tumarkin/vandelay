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
version = Version [0,1,1,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/roberttumarkin/Dropbox/Coding/vandelay/.cabal-sandbox/bin"
libdir     = "/Users/roberttumarkin/Dropbox/Coding/vandelay/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/vandelay-0.1.1.4"
datadir    = "/Users/roberttumarkin/Dropbox/Coding/vandelay/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/vandelay-0.1.1.4"
libexecdir = "/Users/roberttumarkin/Dropbox/Coding/vandelay/.cabal-sandbox/libexec"
sysconfdir = "/Users/roberttumarkin/Dropbox/Coding/vandelay/.cabal-sandbox/etc"

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
