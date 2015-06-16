module App.Vandelay.Template.Parser
  ( readTemplateEIO
  ) where

import Debug.Trace

import Control.Applicative
import Control.Monad -- (MonadPlus(..), ap)
import Data.Maybe
import Data.Monoid

import Text.Parsec.Error
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Prim as P
import qualified Data.Char as C
import qualified Data.Text as T

import App.Vandelay.Types
import App.Vandelay.Template.Types
import App.Vandelay.IO






readTemplateEIO :: String  -- File name
                 -> EIO String VandelayTemplate
readTemplateEIO f = 
  safeReadFileWithError f "Estimates file" >>= hoistEither . parseTemplate
  -- case parse tablefile "Table parsing error: " txt of 
  --   Left  err -> left  $ show err
  --   Right vt  -> right $ vt


traceControl = False

-- PARSE FUNCTION
parseTemplate :: String -> Either String VandelayTemplate
parseTemplate input = 
  case parse tablefile "Table parsing error: " input of 
    Left e  -> case traceControl of
                  True -> traceShow e $ (Left $ show e)
                  False -> Left $ show e 
    Right r -> case traceControl of
                  True -> traceShow r $ Right r 
                  False -> Right r





tablefile :: GenParser Char st VandelayTemplate
tablefile = 
  skipMany blankline *> many section <* eof  -- Generates a list of Vandelay tables 
  >>= return . mconcat -- Concates the Vandelay tables into a single table


section :: GenParser Char st VandelayTemplate 
section = 
      configSection
  <|> tableSection
  <|> subSection 
  <?> "section header (configuration, table, or substitutions)"







-- CONFIGURATION SECTION
configSection  :: GenParser Char st VandelayTemplate
configLine     :: GenParser Char st Configuration
configCommand  :: GenParser Char st Configuration
configDataFile :: GenParser Char st Configuration
configModels   :: GenParser Char st Configuration
configTexfile  :: GenParser Char st Configuration

configSection = do
  _  <- string "configuration:" <* eol 
  cs <- manyTillSectionHeader configLine 
  return $ blankVandelayTemplate{configuration = mconcat cs}

configLine = 
      try ( blankline >> return blankConfiguration)
  <|> space1 *> configCommand 

configCommand = 
      configDataFile 
  <|> configModels   
  <|> configTexfile  
  <?> "Invalid configuration command. Valid commands are \"data:\", \"models:\", and \"tex:\"."
  

configDataFile = basicCommand "data:" (\p -> blankConfiguration{datafile = Last . Just $ p})
configModels   = basicCommand "models:" (\p -> blankConfiguration{desiredModels = Last . Just $ p})
configTexfile  = basicCommand "tex:" (\p -> blankConfiguration{texfile = Last . Just $ p})
 




-- TABLE SECTION
tableSection   :: GenParser Char st VandelayTemplate
tableLine      :: GenParser Char st (Maybe TableCommand)
tableStatement :: GenParser Char st (Maybe TableCommand)

tableSection = do 
  _    <- string "table:" *> eol
  cmds <-  manyTillSectionHeader tableLine 
  return $ blankVandelayTemplate{table = catMaybes cmds}


tableLine =  
      try (blankline >> return Nothing) 
  <|> space1 *> tableStatement 
  

tableStatement = 
      try (basicCommand "template:" (Just . Template))
  <|> try (basicCommand "latex:" (Just . Latex)) 
  <|> try dataRow
  <?> "table output statement (template, latex, code, index, format, name,  scale, and surround)." 


-- DATA COMMAND
dataRow      :: GenParser Char st (Maybe TableCommand)
dataCommands :: GenParser Char st [DataCommand]
dataCommand  :: GenParser Char st DataCommand

dataRow = do
  d <- dataCommands <* eol
  return . Just $ Data (createOutputRequest d)

dataCommands = sepBy dataCommand (char ';')

dataCommand =  do 
  fcn <-  spaces >> dataCommand' 
  ps  <- many (noneOf ";\n")
  return $ fcn (stripStr ps)

dataCommand' = 
      (try (string "scale:") >> return Scale)
  <|> (string "surround:"    >> return Surround)
  <|> (string "name:"        >> return Name) 
  <|> (string "code:"        >> return Code)
  <|> (string "index:"       >> return Index)
  <|> (string "format:"      >> return Format)


-- SUBSTITUTION SECTION
subSection     :: GenParser Char st VandelayTemplate
subCommands    :: GenParser Char st (T.Text, T.Text)
subDeclaration :: GenParser Char st String

subSection = do 
  _    <- string "substitutions:" *> eol 
  cmds <- manyTillSectionHeader subCommands 
  -- cmds <- many subCommands 
  -- _    <- skipMany blankline 
  return $ blankVandelayTemplate{substitutions = cmds}


subCommands = do 
  name <- subDeclaration

  -- Get position and track indent level
  pos  <- sourcePos 
  let indentlevel = sourceColumn pos - 1

  -- Substitutions
  s  <- manyTill anyChar eol 
  ss <- many (try (indentedOrBlankLine indentlevel)) 

  return (stripStr name, T.unlines (stripStr s : map stripStr ss))


subDeclaration = 
  space1 *> many (noneOf ":\n\r") 
    <*  ( (char ':') <?> "Colon is needed to declare text source for substitutions.")










-- Parsec utility functions
basicCommand :: String 
                -> (T.Text -> a) 
                -> GenParser Char st a
basicCommand codestr fcn = 
  (string codestr) *> manyTillEol
  >>= return . fcn . stripStr


space1    = space >> spaces

eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"

sectionHeadOrEof :: GenParser Char st String
sectionHeadOrEof =
      try (letter >> return "")
  <|> (eof >> return "")
  <?> "Expecting section definition or end of file."

manyTillEol             = manyTill anyChar eol 
manyTillSectionHeader x = manyTill x (lookAhead sectionHeadOrEof)

-- Taken from http://stackoverflow.com/questions/27469281/get-current-position-in-parsed-source-using-parsec
sourcePos :: GenParser Char st SourcePos
sourcePos = statePos `liftM` getParserState

parseInt :: GenParser Char st SourcePos 
parseInt = do
  numStr <- many1 digit
  pos    <- sourcePos
  return $ (pos) -- , (read numStr))

-- Text utility functions
stripStr :: String -> T.Text
stripStr = T.strip . T.pack 





blankline = many (char ' ')  *> eol *> return ""

indentedLine :: Int -- Indent level
             -> GenParser Char st String
indentedLine i = count i (char ' ')  -- Use this instead of space to ensure it doesn't include EOL 
  *> manyTill anyChar eol

indentedOrBlankLine i = 
      try ( indentedLine i ) 
  <|> try blankline 




-- -- Debugging and experimenting

-- ct1 = parse configDataFile "Err" " data: test.csv\n"
-- ct2 = parse configCommand "Err" " models: model1, model2\n data: test.csv"
-- ct3 = parse configSection "Err" "configuration:\n models: model1, model2\n data: test.csv\n tex: out.tex\n tex: over.tex\n"

-- -- Config tests
-- ec1 = parse configCommand  "Err" "data: test.csv\n"
-- ec2 = parse configCommand  "Err" "models: modela, model b\n"
-- ec3 = parse configCommand  "Err" "tex: out.tex\n"
-- ec4 = parse configCommand  "Err" "error: out.tex\n"
-- el1 = parse configLine "Err" " data: test.csv\n"
-- el2 = parse configLine "Err" " models: modela, model b\n"
-- el3 = parse configLine "Err" " tex: out.tex\n"
-- el4 = parse configLine "Err" " error: out.tex\n"

-- es1 = parse configSection "Err" "configuration:\n \n data: test.csv\n tex: out.tex\n tex: over.tex\n"
-- es2 = parse configSection "Err" "configuration:\n \n data: test.csv\n tex: out.tex\n    \n\n \n tex: over.tex\n"
-- es3 = parse configSection "Err" "configuration:\n phelan: vendeville\n data: test.csv\n tex: out.tex\n tex: over.tex\n"

-- -- Table tests
-- ts1 = parse tableSection "Err" "table:\n template: template.tex \n"
-- ts2 = parse tableSection "Err" "table:\n latex: \\addblankline\n"
-- ts3 = parse tableSection "Err" "table:\n template: template.tex \n latex: \\addblankline\n"



-- ts4 = parse tablefile "Err" "table:\n template: template.tex \n latex: \\addblankline\nconfiguration:\n  data: test.csv\n tex: out.tex\n    \n\n \n tex: over.tex\n"

















