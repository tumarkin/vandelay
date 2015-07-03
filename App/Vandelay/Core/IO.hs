{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Core.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile

  , unsafeWriteFile

  , globPaths
  ) where

import App.Vandelay.Core.Types
import Data.Bool
import Data.List
import Control.Monad
import System.FilePath.Glob
import System.Directory
import System.IO



unsafeWriteFile :: Maybe String -- Optional output filename 
                -> String       -- The text to output
                -> EIO String ()
unsafeWriteFile fo t = do
  h <- unsafeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h


safeReadFileWithError :: String -> String -> EIO String String
safeReadFileWithError f e = do 
  exists <- liftIO . doesFileExist $ f
  if exists then liftIO (readFile f) 
            else left $ unwords [e,f,"not found."]

safeReadFile :: String -> EIO String String
safeReadFile f = safeReadFileWithError f "File"


safeWriteFile :: Maybe String -- Optional output filename 
              -> String       -- The text to output
              -> EIO String ()
safeWriteFile fo t = do
  h <- safeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h

safeGetHandle :: Maybe String -> EIO String Handle
safeGetHandle Nothing = return stdout 
safeGetHandle (Just t)= 
  (liftIO . doesFileExist $ t)
  >>= bool (unsafeGetHandle (Just t))  -- Does not exist
           (overwriteHandle t)         -- Exists

overwriteHandle :: String -> EIO String Handle
overwriteHandle t = do
  liftIO . putStrLn $ unwords ["File",t,"exists. Overwrite (y/n)?"]
  ans <- liftIO getLine
  if ans == "y" then unsafeGetHandle $ Just t
                else left "Execution halted."


unsafeGetHandle  :: Maybe String -> EIO String Handle
unsafeGetHande  Nothing  = return stdout
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



globPaths :: [String] -> EIO String [FilePath]
globPaths = liftM (nub . concat) . sequence . map safeGlob


safeGlob :: String  -> EIO String [FilePath]
safeGlob s = do
  gs <- liftIO . glob $ s
  case gs of 
    [] -> left $ unwords ["No files found matching pattern",s]
    gs -> right gs




