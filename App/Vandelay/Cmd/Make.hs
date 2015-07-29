{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Make
  ( makeTables
  , makeTable
  ) where

import Control.Monad.Trans.RWS
import Rainbow

import App.Vandelay.Core 
import App.Vandelay.Estimates
import App.Vandelay.Template



makeTables :: [String]        -- ^ Vandelay template filepath globs
           -> EIO ErrorMsg () -- ^ Error message or () 
makeTables gs = do
  globs <- globPaths gs
  mapM_ makeTable globs

-- | Create a LaTeX table from a Vandelay template
makeTable :: String          -- ^ Vandelay template filepath
          -> EIO ErrorMsg () -- ^ Error message or () 
makeTable templatePath = do 
  template  <- readTemplateEIO templatePath
  outFile   <- hoistEither . safeGetTexfile $ template
  (_,_,res) <- runMakeMonad createOutput template

  liftIO . putChunk $ ( "Success: " <> fore green <> bold) 
  liftIO . putStrLn $ templatePath
  unsafeWriteFile (Just outFile) res


-- | Internal data types 
type MakeMonad      = RWST VandelayTemplate String () (EIO ErrorMsg)
runMakeMonad mm vtl = runRWST mm vtl ()

askTable         :: MakeMonad [TableCommand]
askDesiredModels :: MakeMonad [String]
askSubstitutions :: MakeMonad [(Text, Text)]
askEstimates     :: MakeMonad [Estimates]

askTable         = asks table
askDesiredModels = lift . hoistEither . safeGetDesiredModels =<< ask
askSubstitutions = asks substitutions
askEstimates     = lift . hoistEither . safeGetEstimates =<< ask

-- | Table output creation functions 
createOutput :: MakeMonad () 
createOutput =  mapM_ doTableCommand =<< askTable

doTableCommand :: TableCommand -> MakeMonad () 
doTableCommand (Latex l)    = tellLn $ l -- ++ "\\\\" 
doTableCommand (Template t) = tellLn =<< return doSubstitution `ap` (lift . safeReadFile $ t) `ap` askSubstitutions
doTableCommand (Data   or)  = tellLn =<< lift . hoistEither =<< return outputRow `ap` return or `ap` askEstimates `ap` askDesiredModels


-- | Text utility functions
tellLn s = tell $ s ++ "\n"


