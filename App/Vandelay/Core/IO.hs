{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Core.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile

  , unsafeWriteFile

  , globPaths
  ) where

import Data.Bool
import System.FilePath.Glob
import System.Directory
import System.IO

import App.Vandelay.Core.Types
import App.Vandelay.Core.Modules



unsafeWriteFile :: Maybe String -- Optional output filename 
                -> String       -- The text to output
                -> EIO ErrorMsg ()
unsafeWriteFile fo t = do
  h <- unsafeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h


safeReadFileWithError :: String -> String -> EIO ErrorMsg String
safeReadFileWithError f e = do 
  exists <- liftIO . doesFileExist $ f
  if exists then liftIO (readFile f) 
            else left $ fileNotFoundMsg f e 

safeReadFile :: String -> EIO ErrorMsg String
safeReadFile f = safeReadFileWithError f "File"


safeWriteFile :: Maybe String -- Optional output filename 
              -> String       -- The text to output
              -> EIO ErrorMsg ()
safeWriteFile fo t = do
  h <- safeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h

safeGetHandle :: Maybe String -> EIO ErrorMsg Handle
safeGetHandle Nothing = return stdout 
safeGetHandle (Just t)= 
  (liftIO . doesFileExist $ t)
  >>= bool (unsafeGetHandle (Just t))  -- Does not exist
           (overwriteHandle t)         -- Exists

overwriteHandle :: String -> EIO ErrorMsg Handle
overwriteHandle t = do
  liftIO . putStrLn $ unwords ["File",t,"exists. Overwrite (y/n)?"]
  ans <- liftIO getLine
  if ans == "y" then unsafeGetHandle $ Just t
                else left userHaltMessage 


unsafeGetHandle  :: Maybe String -> EIO ErrorMsg Handle
unsafeGetHandle Nothing  = return stdout
unsafeGetHandle (Just t) = liftIO $ openFile t WriteMode 




safeCloseHandle :: Handle -> IO ()
safeCloseHandle h | h == stdout = return ()
                  | otherwise   = hClose h

-- outputSuccess :: (String, Handle) -> IO ()
-- outputSuccess (result, h) 
--     | h == stdout = putStrLn result
--     | otherwise   = hPutStrLn h result 
--     >> putChunkLn ( "Vandelay INSERT JOKE HERE." <> fore green <> bold) 
--     >> hClose h


-- globPaths :: [String] -> IO [FilePath]
-- globPaths = liftM (nub . concat) . sequence . map glob



globPaths :: [String] -> EIO ErrorMsg [FilePath]
globPaths = liftM (nub . concat) . mapM safeGlob


safeGlob :: String  -> EIO ErrorMsg [FilePath]
safeGlob s = do
  gs <- liftIO . glob $ s
  case gs of 
    [] -> left $ globPatternErrorMsg s
    gs -> right gs




-- Error Messages
fileNotFoundMsg f e = unwords [e,f,"not found."]
userHaltMessage     = "Execution halted."
globPatternErrorMsg s = unwords ["No files found matching pattern",s]
