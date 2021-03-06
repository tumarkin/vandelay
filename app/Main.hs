import qualified Data.Text                     as T
import           Options.Applicative
import           Options.Applicative.Builder   (readerError)
import           Rainbow                       hiding ((<>))
import qualified Rainbow.Translate             as RT

import           Vandelay.App.Cmd.Init
import           Vandelay.App.Cmd.Make
import           Vandelay.App.Template.ParserT
import           Vandelay.DSL.Core
import           Vandelay.DSL.Estimates



--------------------------------------------------------------------------------
-- Program                                                                    --
--------------------------------------------------------------------------------

main ∷ IO ()
main = 
  run =<< execParser
        (parseCommand `withInfo` "Generate LaTeX tables")
  
run ∷ Command → IO ()
run cmd = do

  -- Get an EitherT IO as the result
  let resultEIO = case cmd of
                    Init files out sort dfr -> initTemplate files out sort dfr
                    Make files outputPath   -> makeTables   outputPath files

    -- Capture the result of type Either String (String, Handle)
  runExceptT resultEIO >>= \case
    Left err  -> RT.putChunkLn (Rainbow.chunk (asText "Vandelay error:") & fore red)
              >> putStrLn err
    Right _   -> return ()


--------------------------------------------------------------------------------
-- Commands                                                                   --
--------------------------------------------------------------------------------
data Command
    = Init [File] Output SortOptions SourceFileReferences
    | Make [File] OutputPath
    deriving (Show)

type File       = String
type Output     = Maybe String
type OutputPath = String


--------------------------------------------------------------------------------
-- Option parsing                                                             --
--------------------------------------------------------------------------------
withInfo ∷ Parser a → String → ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand ∷ Parser Command
parseCommand = subparser $
    command "init" (parseInit `withInfo` "Create a blank template from a tab-separated results file(s)") <>
    command "make" (parseMake `withInfo` "Generate LaTeX from a template file(s)")



parseInit ∷ Parser Command
parseInit = Init
    <$> some (argument str (metavar "TAB-SEPARATED-FILE(S)"))
    <*> parseOutput
    <*> parseSortOptions
    <*> parseSourceFileReferences

parseMake ∷ Parser Command
parseMake = Make
    <$> some (argument str (metavar "VANDELAY-TEMPLATE(S)"))
    <*> outputPath
  where
    outputPath = strOption ( long "output-path"
                           <> short 'o'
                           <> metavar "PATH"
                           <> help "Destination for the processed templates"
                           <> value "."
                           )
        
     


-- Initialization options

parseOutput ∷ Parser Output
parseOutput =
  optional $ strOption ( long "output"
                       <> short 'o'
                       <> metavar "FILENAME"
                       <> help "Save initialization template file"
                       )

parseSourceFileReferences ∷ Parser SourceFileReferences
parseSourceFileReferences = option (str >>= readSourceFileReferences) ( long "include-source-file-references"
                                  <> short 'i'
                                  <> help "Include source file references for model and variable cross-referencing."
                                  <> value NoSFR -- Default value
                            )

readSourceFileReferences ∷ String → ReadM SourceFileReferences
readSourceFileReferences s | toLower s `elem` ["f","full"]         = return FullPath
                           | toLower s `elem` ["a","abbreviated"]  = return Abbreviation
                           | otherwise                               = readerError $ unwords ["Source file referencing option", s, "not recognized. Use (F)ull or (A)bbreviated."]

parseSortOptions = SortOptions
    <$> parseSortModels
    <*> parseSortVars

parseSortModels ∷ Parser Bool
parseSortModels = switch ( long "no-sort-models"
                       <> short 'm'
                       <> hidden
                       <> help "Sort models by order of appearance instead of alphabetically."
                       )
 

parseSortVars ∷ Parser Bool
parseSortVars = switch ( long "no-sort-vars"
                       <> short 'v'
                       <> hidden
                       <> help "Sort variables by order of appearance instead of alphabetically."
                       )







