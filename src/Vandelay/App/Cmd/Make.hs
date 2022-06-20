module Vandelay.App.Cmd.Make
  ( makeTables
  , makeTable
  ) where

import           Control.Monad.Trans.RWS hiding (ask, asks)
import           Prelude                 (putStrLn)
import           Rainbow
import qualified Rainbow.Translate       as RT
import           RIO.FilePath
import qualified RIO.Text                as T
import           Vandelay.DSL.Core
import           Vandelay.DSL.Estimates
import           Vandelay.App.Template.IO



makeTables
    ∷ FilePath        -- ^ Output directory
    → [String]        -- ^ Vandelay template filepath globs
    → ExceptT ErrorMsg (RIO env) () -- ^ Error message or ()
makeTables dir gs =
  mapM_ (makeTable dir) =<< globPaths gs

-- | Create a LaTeX table from a Vandelay template
makeTable
    ∷ FilePath        -- ^ Output directory
    → String          -- ^ Vandelay template filepath
    → ExceptT ErrorMsg (RIO env) () -- ^ Error message or ()
makeTable dir templatePath = addFilepathIfError $ do
    template  <- readTemplate templatePath
    let outFile = dir </> takeFileName templatePath -<.> "tex"
    (_,_,res) <- runMakeMonad createOutput template

    liftIO . RT.putChunk $ Rainbow.chunk "Success: " & fore green
    liftIO . putStrLn $ templatePath
    unsafeWriteFile (Just outFile) res

  where
    addFilepathIfError = prependError ("In template: " <> tTemplatePath <> "\n")
    tTemplatePath = T.pack templatePath


-- | Internal data types
type MakeMonad env  = RWST VandelayTemplate Text () (ExceptT ErrorMsg (RIO env))
runMakeMonad mm vtl = runRWST mm vtl ()

askTable         ∷ MakeMonad env [TableCommand]
askDesiredModels ∷ MakeMonad env [(FilePath, Text)]
askSubstitutions ∷ MakeMonad env [(Text, Text)]
askEstimatesHM   ∷ MakeMonad env EstimatesHM

askTable         = asks table
askDesiredModels = hoistEitherError . getDesiredModels =<< ask
askSubstitutions = asks substitutions
askEstimatesHM   = hoistEitherError . getEstimatesHM =<< ask

-- | Table output creation functions
createOutput ∷ MakeMonad env ()
createOutput =  mapM_ doTableCommand =<< askTable

doTableCommand ∷ TableCommand → MakeMonad env ()
doTableCommand (Latex l)    = tellLn l
-- doTableCommand (Template t) = tellLn =<< doSubstitution <$> (lift . safeReadFile $ t) <*> askSubstitutions
doTableCommand (Data   or)  = tellLn =<< hoistEitherError =<< outputRow or <$> askEstimatesHM <*> askDesiredModels


-- | Text utility functions
tellLn ∷ Text → MakeMonad env ()
tellLn s = tell $ s <> "\n"


