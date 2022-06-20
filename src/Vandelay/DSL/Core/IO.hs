{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vandelay.DSL.Core.IO
  ( safeReadFile
  , safeReadFileWithError
  , safeWriteFile

  , unsafeWriteFile

  , globPaths
  ) where

import qualified RIO.Set as Set
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Text.IO              (getLine, hPutStrLn)
import           Prelude                   (putStrLn)
import           RIO.Directory
import qualified RIO.Text                  as T
import           System.FilePath.Glob
import           System.IO                 (IOMode (..), openFile)
import           Vandelay.DSL.Core.Modules as VC



unsafeWriteFile ∷ (MonadError ErrorMsg m, MonadIO m)
                ⇒ Maybe FilePath -- Optional output filename
                → Text           -- The text to output
                → m ()
unsafeWriteFile fo t = do
  h <- unsafeGetHandle fo
  liftIO $ hPutStrLn h t >> safeCloseHandle h


safeReadFileWithError ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  FilePath → Text → m Text
safeReadFileWithError f e = do
  exists <- liftIO . doesFileExist $ f
  if exists then liftIO (readFileUtf8 f)
            else throwError $ fileNotFoundMsg f e

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
  liftIO . putStrLn . T.unpack $ T.unwords ["File",T.pack t,"exists. Overwrite (y/n)?"]
  ans <- liftIO getLine
  if ans == "y" then unsafeGetHandle $ Just t
                else throwError userHaltMessage


unsafeGetHandle  ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  Maybe FilePath → m Handle
unsafeGetHandle Nothing  = return stdout
unsafeGetHandle (Just t) = liftIO $ VC.openFile t WriteMode




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
fileNotFoundMsg f e = T.unwords [e, T.pack f, "not found."]

userHaltMessage ∷ Text
userHaltMessage = "Execution halted."

globPatternErrorMsg ∷ FilePath → Text
globPatternErrorMsg s = T.unwords ["No files found matching pattern", T.pack s]





----------------------------------------------------------------------------------------------------
-- Utility functions                                                                              --
----------------------------------------------------------------------------------------------------
ordNub :: forall a. (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go :: Set.Set a -> [a] -> [a]
    go _ []     = []
    go s (x:xs) =
        if x `Set.member` s
        then go s xs
        else x : go (Set.insert x s) xs
