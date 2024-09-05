import           Options.Applicative
import           Rainbow                hiding ((<>))
import qualified Rainbow.Translate      as RT
import RIO

import           Vandelay.App.Cmd.Dhall
import           Vandelay.App.Cmd.Init
import           Vandelay.App.Cmd.Make
import           Vandelay.DSL.Core

--------------------------------------------------------------------------------
-- Program                                                                    --
--------------------------------------------------------------------------------

main ∷ IO ()
main = printError =<< run =<< execParser (parseCommand `withInfo` "Generate LaTeX tables")

run ∷ Command → IO (Either Text ())
run cmd =
  runSimpleApp . runExceptT $
      case cmd of
        Init files out        -> initTemplate files out
        Make files outputPath -> makeTables   outputPath files
        Dhall      outputPath -> lift $ installLibrary outputPath

      -- liftIO $ printError resultEIO


printError (Right _)  = pure ()
printError (Left err) = RT.putChunkLn (Rainbow.chunk "Vandelay error:" & fore red)
                        >> RT.putChunkLn (Rainbow.chunk err)
--------------------------------------------------------------------------------
-- Commands                                                                   --
--------------------------------------------------------------------------------
data Command
    = Init [File] Output -- SortOptions -- SourceFileReferences
    | Make [File] OutputPath
    | Dhall OutputPath
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
    command "init"  (parseInit  `withInfo` "Create a blank template from a tab-separated results file(s)") <>
    command "make"  (parseMake  `withInfo` "Generate LaTeX from a template file(s)") <>
    command "dhall" (parseDhall `withInfo` "Install DHALL library")



parseInit ∷ Parser Command
parseInit = Init
    <$> some (argument str (metavar "TAB-SEPARATED-FILE(S)"))
    <*> parseOutput

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

parseDhall ∷ Parser Command
parseDhall = Dhall
    <$> argument str (metavar "DHALL-LIBRARY-PATH")




-- Initialization options

parseOutput ∷ Parser Output
parseOutput =
  optional $ strOption ( long "output"
                       <> short 'o'
                       <> metavar "FILENAME"
                       <> help "Save initialization template file"
                       )

