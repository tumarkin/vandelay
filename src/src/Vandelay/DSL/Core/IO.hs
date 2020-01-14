module Vandelay.DSL.Core.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile

  , unsafeWriteFile

  , globPaths
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Text.IO              (hPutStrLn)
import           System.Directory
import           System.FilePath.Glob
import           System.IO                 (IOMode (..), openFile)

import           Vandelay.DSL.Core.Modules
import           Vandelay.DSL.Core.Types



unsafeWriteFile ∷ (MonadError ErrorMsg m, MonadIO m)
                ⇒ Maybe FilePath -- Optional output filename
                → Text           -- The text to output
                → m ()
unsafeWriteFile fo t = do
  h <- unsafeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h


safeReadFileWithError ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  FilePath → Text → m Text
safeReadFileWithError f e = do
  undefined
  -- exists <- liftIO . doesFileExist $ f
  -- if exists then liftIO (readFileUtf8 f)
  --           else throwError $ fileNotFoundMsg f e

safeReadFile ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ FilePath → m Text
safeReadFile f = safeReadFileWithError f "File"


safeWriteFile ∷ (MonadError ErrorMsg m, MonadIO m) 
              ⇒  Maybe FilePath -- Optional output filename
              → Text           -- The text to output
              → m ()
safeWriteFile fo t = do
  h <- safeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h

safeGetHandle ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  Maybe FilePath → m Handle
safeGetHandle Nothing = return stdout
safeGetHandle (Just t)=
  (liftIO . doesFileExist $ t)
  >>= bool (unsafeGetHandle (Just t))  -- Does not exist
           (overwriteHandle t)         -- Exists

overwriteHandle ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  FilePath → m Handle
overwriteHandle t = do
  liftIO . putStrLn $ unwords ["File",pack t,"exists. Overwrite (y/n)?"]
  ans <- liftIO getLine
  if ans == "y" then unsafeGetHandle $ Just t
                else throwError userHaltMessage


unsafeGetHandle  ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  Maybe FilePath → m Handle
unsafeGetHandle Nothing  = return stdout
unsafeGetHandle (Just t) = liftIO $ openFile t WriteMode




safeCloseHandle ∷ Handle → IO ()
safeCloseHandle h | h == stdout = return ()
                  | otherwise   = hClose h



globPaths ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  [String] → m [FilePath]
globPaths = fmap (ordNub . concat) . mapM safeGlob


safeGlob ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  String  → m [FilePath]
safeGlob s =
  (liftIO . glob $ s) >>= \case
    [] → throwError $ globPatternErrorMsg s
    gs → return gs




-- Error Messages
fileNotFoundMsg ∷ FilePath → Text → Text
fileNotFoundMsg f e = unwords [e, pack f, "not found."]

userHaltMessage ∷ Text
userHaltMessage = "Execution halted."

globPatternErrorMsg ∷ FilePath → Text
globPatternErrorMsg s = unwords ["No files found matching pattern", pack s]





