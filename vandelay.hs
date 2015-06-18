{-# LANGUAGE OverloadedStrings #-}

import App.Vandelay.Cmd
import App.Vandelay.Estimates
import App.Vandelay.IO
import App.Vandelay.Template 
import App.Vandelay.Text (stripSplitCommas)
import App.Vandelay.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Monoid
import Options.Applicative -- Provided by optparse-applicative
import Rainbow
import qualified Data.Text as T

import Debug.Trace


data Command
    = Init File Output SortOptions
    | Make File 
    deriving (Show)

type File       = String
type Output     = Maybe String






withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
    command "init" (parseInit `withInfo` "Create a blank template from a tab-separated results file") <>
    command "make" (parseMake `withInfo` "Generate LaTeX from a template file") 
    


parseInit :: Parser Command
parseInit = Init
    <$> argument str (metavar "TAB-SEPARATED-FILE")
    <*> parseOutput
    <*> parseSortOptions

parseMake :: Parser Command
parseMake = Make <$> argument str (metavar "VANDELAY-TEMPLATE")



-- Initialization options

parseOutput :: Parser Output
parseOutput = 
  optional $ strOption ( long "output"
                       <> short 'o'
                       <> metavar "FILENAME"
                       <> help "Save initialization template file"
                       )

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
                      Init file out sort -> initTemplate file out sort
                      Make file          -> makeTable file

    -- Capture the result of type Either String (String, Handle)
    result <- runEitherT resultEIO

    case result of 
      Left err  -> putChunkLn ( "Vandelay error:" <> fore red <> bold) 
                >> putStrLn err
      Right _   -> return () 


main :: IO ()
main = do
  run =<< execParser 
        (parseCommand `withInfo` "Generate LaTeX tables")
  return ()





















-- Initialization test success and failure
its = run (Init "test/activity_delegation_fully_combined.txt" Nothing (SortOptions True False))
itf = run (Init "test/activity_delegation_fully_combined.txasdt" Nothing (SortOptions True False))



art = run (Make "test/vl.acquisition_returns.yaml") 

