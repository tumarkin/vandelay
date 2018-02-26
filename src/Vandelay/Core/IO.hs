module Vandelay.Core.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile

  , unsafeWriteFile

  , globPaths
  ) where

import           Data.Bool
import           System.Directory
import           System.FilePath.Glob
import           System.IO                 (IOMode (..), openFile)
import           Data.Text.IO              (hPutStrLn)

import           Vandelay.Core.Modules
import           Vandelay.Core.Types



unsafeWriteFile ∷ Maybe FilePath -- Optional output filename
                → Text           -- The text to output
                → EIO ErrorMsg ()
unsafeWriteFile fo t = do
  h <- unsafeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h


safeReadFileWithError ∷ FilePath → Text → EIO ErrorMsg Text
safeReadFileWithError f e = do
  exists <- liftIO . doesFileExist $ f
  if exists then liftIO (readFileUtf8 f)
            else throwError $ fileNotFoundMsg f e

safeReadFile ∷ FilePath → EIO ErrorMsg Text
safeReadFile f = safeReadFileWithError f "File"


safeWriteFile ∷ Maybe FilePath -- Optional output filename
              → Text           -- The text to output
              → EIO ErrorMsg ()
safeWriteFile fo t = do
  h <- safeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h

safeGetHandle ∷ Maybe FilePath → EIO ErrorMsg Handle
safeGetHandle Nothing = return stdout
safeGetHandle (Just t)=
  (liftIO . doesFileExist $ t)
  >>= bool (unsafeGetHandle (Just t))  -- Does not exist
           (overwriteHandle t)         -- Exists

overwriteHandle ∷ FilePath → EIO ErrorMsg Handle
overwriteHandle t = do
  liftIO . putStrLn $ unwords ["File",pack t,"exists. Overwrite (y/n)?"]
  ans <- liftIO getLine
  if ans == "y" then unsafeGetHandle $ Just t
                else throwError userHaltMessage


unsafeGetHandle  ∷ Maybe FilePath → EIO ErrorMsg Handle
unsafeGetHandle Nothing  = return stdout
unsafeGetHandle (Just t) = liftIO $ openFile t WriteMode




safeCloseHandle ∷ Handle → IO ()
safeCloseHandle h | h == stdout = return ()
                  | otherwise   = hClose h



globPaths ∷ [String] → EIO ErrorMsg [FilePath]
globPaths = fmap (ordNub . concat) . mapM safeGlob


safeGlob ∷ String  → EIO ErrorMsg [FilePath]
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





