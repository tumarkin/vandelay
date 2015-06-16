{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile
  ) where

import App.Vandelay.Types 
import App.Vandelay.Text 
import Data.Bool
import System.Directory
import System.IO
import Rainbow



safeReadFileWithError :: String -> String -> EIO String String
safeReadFileWithError f e = do 
  exists <- liftIO . doesFileExist $ f
  case exists of 
    True  -> liftIO (readFile f) 
    False -> left $ unwords [e,f,"not found."]

safeReadFile :: String -> EIO String String
safeReadFile f = safeReadFileWithError f "File"


safeWriteFile :: Maybe Text -- Optional output filename 
              -> Text       -- The text to output
              -> EIO String ()
safeWriteFile fo t = do
  h <- safeGetHandle fo
  liftIO $ (hPutStrLn h (unpack t)) 
           >> safeCloseHandle h







safeGetHandle :: Maybe Text -> EIO String Handle
safeGetHandle Nothing = return stdout 
safeGetHandle (Just t)= 
  (liftIO . doesFileExist $ (unpack t))
  >>= bool (unsafeGetHandle t)  -- Does not exist
           (overwriteHandle t)  -- Exists

overwriteHandle :: Text -> EIO String Handle
overwriteHandle t = do
  liftIO . putStrLn $ unwords ["File",unpack t,"exists. Overwrite (y/n)?"]
  ans <- liftIO (getLine)
  if ans == "y" then unsafeGetHandle t
                else left "Execution halted."


unsafeGetHandle  :: Text -> EIO String Handle
unsafeGetHandle t = liftIO $ openFile (unpack t) WriteMode 




safeCloseHandle :: Handle -> IO ()
safeCloseHandle h | h == stdout = return ()
                  | otherwise   = hClose h

-- outputSuccess :: (String, Handle) -> IO ()
-- outputSuccess (result, h) 
--     | h == stdout = putStrLn result
--     | otherwise   = hPutStrLn h result 
--     >> putChunkLn ( "Vandelay INSERT JOKE HERE." <> fore green <> bold) 
--     >> hClose h

