module App.Vandelay.Cmd.Make                                      
  ( makeTable
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Either

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
askDesiredModels = lift . hoistEither . safeGetDesiredModels =<< ask


askSubstitutions :: MakeMonad [(Text, Text)] 
askSubstitutions =  asks substitutions

askEstimates :: MakeMonad [Estimates]
askEstimates =  lift . hoistEither . safeGetEstimates =<< ask

-- Table output creation functions 
createOutput :: MakeMonad () 
createOutput =  mapM_ doTableCommand =<< askTable

doTableCommand :: TableCommand -> MakeMonad () 
doTableCommand (Latex l)    = tellLn $ l ++ "\\\\" 
doTableCommand (Template t) = tellLn =<< return doSubstitution `ap` (lift . safeReadFile $ t) `ap` askSubstitutions
doTableCommand (Data   or)  = tellLn =<< lift . hoistEither =<< return outputRow `ap` return or `ap` askEstimates `ap` askDesiredModels
                                                                -- Gives a Monad (Either String String) 
                                      --  MakeMonadEither <- Either T <- Either String String 


tellLn s = tell $ s ++ "\n"


