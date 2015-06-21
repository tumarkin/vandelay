module App.Vandelay.Cmd.Make
  ( makeTable
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import App.Vandelay.Estimates
import App.Vandelay.IO
import App.Vandelay.Template
import App.Vandelay.Text 
import App.Vandelay.Types


makeTable :: String -> EIO String () -- String, Handle)
makeTable templatePath = do 
  template  <- readTemplateEIO templatePath
  outFile   <- hoistEither . safeGetTexfile $ template
  (_,_,res) <- runMakeMonad createOutput template

  safeWriteFile (Just outFile) res


--- Internal data types 
type MakeMonad = RWST VandelayTemplate String () (EIO String)

runMakeMonad mm vtl = runRWST mm vtl () 

askTable :: MakeMonad [TableCommand]
askTable = asks table 


askDesiredModels :: MakeMonad [String]
askDesiredModels = do
  vt <- ask
  dms  <- lift . hoistEither $ safeGetDesiredModels vt
  return dms

-- This is super klugey and should be fixed
askOutputKluge :: MakeMonad (Estimates, [String])
askOutputKluge =  return (,) `ap` askEstimates `ap`  askDesiredModels

askSubstitutions :: MakeMonad [(Text, Text)] 
askSubstitutions =  asks substitutions

askEstimates :: MakeMonad Estimates
askEstimates =  asks (fromJust . estimates) 

-- Table output creation functions 
createOutput :: MakeMonad () 
createOutput =  mapM_ doTableCommand =<< askTable

doTableCommand :: TableCommand -> MakeMonad () 
doTableCommand (Latex l)          = tellLn $ l ++ "\\\\" 
doTableCommand (Template t)       = tellLn =<< return doSubstitution `ap` (lift . safeReadFile $ t) `ap` askSubstitutions
doTableCommand (Data   or)        = tellLn =<< lift . outputRowEIO or =<< askOutputKluge 




tellLn s = tell $ s ++ "\n"

