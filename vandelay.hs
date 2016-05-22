import App.Vandelay.Cmd
import App.Vandelay.Estimates
import App.Vandelay.Template 
import App.Vandelay.Core 

import Options.Applicative -- Provided by optparse-applicative
import Options.Applicative.Builder (readerError)
import Rainbow
import qualified Rainbow.Translate as RT

import qualified Data.Text as T


data Command
    = Init [File] Output SortOptions SourceFileReferences
    | Make [File] 
    deriving (Show)

type File       = String
type Output     = Maybe String




withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
    command "init" (parseInit `withInfo` "Create a blank template from a tab-separated results file(s)") <>
    command "make" (parseMake `withInfo` "Generate LaTeX from a template file(s)") 
    


parseInit :: Parser Command
parseInit = Init
    <$> some (argument str (metavar "TAB-SEPARATED-FILE(S)"))
    <*> parseOutput
    <*> parseSortOptions
    <*> parseSourceFileReferences

parseMake :: Parser Command
parseMake = Make <$> some (argument str (metavar "VANDELAY-TEMPLATE(S)"))


-- Initialization options

parseOutput :: Parser Output
parseOutput = 
  optional $ strOption ( long "output"
                       <> short 'o'
                       <> metavar "FILENAME"
                       <> help "Save initialization template file"
                       )

parseSourceFileReferences :: Parser SourceFileReferences
parseSourceFileReferences = option (str >>= readSourceFileReferences) ( long "include-source-file-references"
                                  <> short 'i'
                                  <> help "Include source file references for model and variable cross-referencing."
                                  <> value NoSFR -- Default value
                            )

readSourceFileReferences :: String -> ReadM SourceFileReferences
readSourceFileReferences s | lowercase s `elem` ["f","full"]         = return FullPath
                           | lowercase s `elem` ["a","abbreviated"]  = return Abbreviation
                           | otherwise                               = readerError $ unwords ["Source file referencing option", s, "not recognized. Use (F)ull or (A)bbreviated."]

parseSortOptions = SortOptions
    <$> parseSortModels
    <*> parseSortVars

parseSortModels :: Parser Bool
parseSortModels = switch ( long "no-sort-models"
                       <> short 'm'
                       <> hidden
                       <> help "Sort models by order of appearance instead of alphabetically."
                       )


parseSortVars :: Parser Bool
parseSortVars = switch ( long "no-sort-vars"
                       <> short 'v'
                       <> hidden
                       <> help "Sort variables by order of appearance instead of alphabetically."
                       )







-- Actual program logic
run :: Command -> IO ()
run cmd = do

    -- Get an EitherT IO as the result
    let resultEIO = case cmd of
                      Init files out sort dfr -> initTemplate files out sort dfr
                      Make files              -> makeTables   files

    -- Capture the result of type Either String (String, Handle)
    result <- runEitherT resultEIO

    case result of 
      Left err  -> RT.putChunkLn (Rainbow.chunk "Vandelay error:" & fore red)
                >> putStrLn err
      Right _   -> return () 



main :: IO ()
main = do
  run =<< execParser 
        (parseCommand `withInfo` "Generate LaTeX tables")
  return ()


