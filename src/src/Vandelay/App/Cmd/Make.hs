module Vandelay.App.Cmd.Make
  ( makeTables
  , makeTable
  ) where

import           Control.Monad.Trans.RWS       hiding (ask, asks)
import           Rainbow
import qualified Rainbow.Translate             as RT

import           Vandelay.App.Template.ParserT
import           Vandelay.DSL.Core
import           Vandelay.DSL.Estimates



makeTables ∷ [String]        -- ^ Vandelay template filepath globs
           → EIO ErrorMsg () -- ^ Error message or ()
makeTables gs = do
  globs <- globPaths gs
  mapM_ makeTable globs

-- | Create a LaTeX table from a Vandelay template
makeTable ∷ String          -- ^ Vandelay template filepath
          → EIO ErrorMsg () -- ^ Error message or ()
makeTable templatePath = addFilepathIfError $ do
  template  <- readTemplate templatePath
  outFile   <- hoistEitherError . getTexfile $ template
  (_,_,res) <- runMakeMonad createOutput template

  liftIO . RT.putChunk $ Rainbow.chunk (asText "Success: ") & fore green
  liftIO . putStrLn $ tTemplatePath
  unsafeWriteFile (Just outFile) res

  where
    addFilepathIfError = prependError ("In template: " ++ tTemplatePath ++ "\n")
    tTemplatePath = pack templatePath


-- | Internal data types
type MakeMonad      = RWST VandelayTemplate Text () (EIO ErrorMsg)
runMakeMonad mm vtl = runRWST mm vtl ()

askTable         ∷ MakeMonad [TableCommand]
askDesiredModels ∷ MakeMonad [(Maybe FilePath, Text)]
askSubstitutions ∷ MakeMonad [(Text, Text)]
askEstimatesHM   ∷ MakeMonad EstimatesHM

askTable         = asks table
askDesiredModels = hoistEitherError . getDesiredModels =<< ask
askSubstitutions = asks substitutions
askEstimatesHM   = hoistEitherError . getEstimatesHM =<< ask

-- | Table output creation functions
createOutput ∷ MakeMonad ()
createOutput =  mapM_ doTableCommand =<< askTable

doTableCommand ∷ TableCommand → MakeMonad ()
doTableCommand (Latex l)    = tellLn l
doTableCommand (Template t) = tellLn =<< doSubstitution <$> (lift . safeReadFile $ t) <*> askSubstitutions
doTableCommand (Data   or)  = tellLn =<< hoistEitherError =<< outputRow or <$> askEstimatesHM <*> askDesiredModels


-- | Text utility functions
tellLn ∷ Text → MakeMonad ()
tellLn s = tell $ s ++ "\n"


