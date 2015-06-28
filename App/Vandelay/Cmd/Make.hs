module App.Vandelay.Cmd.Make                                      
  ( makeTable
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import App.Vandelay.Core 
import App.Vandelay.Estimates
import App.Vandelay.Template



-- | Create a LaTeX table from a Vandelay template
makeTable :: String        -- ^ Vandelay template filepath
          -> EIO String () -- ^ Error message or () 
makeTable templatePath = do 
  template  <- readTemplateEIO templatePath
  outFile   <- hoistEither . safeGetTexfile $ template
  (_,_,res) <- runMakeMonad createOutput template

  safeWriteFile (Just outFile) res


--- Internal data types 
type MakeMonad      = RWST VandelayTemplate String () (EIO String)
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
doTableCommand (Latex l)    = tellLn $ l ++ "\\\\" 
doTableCommand (Template t) = tellLn =<< return doSubstitution `ap` (lift . safeReadFile $ t) `ap` askSubstitutions
doTableCommand (Data   or)  = tellLn =<< lift . hoistEither =<< return outputRow `ap` return or `ap` askEstimates `ap` askDesiredModels


-- | Text utility functions
tellLn s = tell $ s ++ "\n"


