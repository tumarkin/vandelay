module App.Vandelay.Template.ParserT
  ( readTemplateEIO
  ) where

import Control.Applicative 
import Control.Monad 
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Parsec hiding (many, optional, (<|>))
import qualified Data.Text as T

import App.Vandelay.Estimates
import App.Vandelay.IO
import App.Vandelay.Template.Types
import App.Vandelay.Text
import App.Vandelay.Types

--- Debugging related
import Debug.Trace
traceControl = False


--- External interface
readTemplateEIO :: String  -- File name
                 -> EIO String VandelayTemplate
readTemplateEIO f = do
  txt <- safeReadFileWithError f "Template file"
  t   <- runParserT tablefile defaultOutputRequest ("Template file: " ++ f) txt 

  case t of 
    Left e  -> if traceControl then traceShow e $ left (show e)
                               else left $ show e 
    Right r -> if traceControl then traceShow r $ right r
                               else right r 
    -- Left  parseErr -> left $ show parseErr
    -- Right t        -> right $ t



--- Parser
type TemplateParser    = ParsecT String UserState (EIO String)

type LastOutputRequest = OutputRequest
type UserState         = LastOutputRequest


tablefile :: TemplateParser VandelayTemplate
tablefile = 
  -- skipMany blankline *> many section <* eof -- Generates a list of Vandelay templates 
  -- >>= return . mconcat                      -- Concates the Vandelay template monoids into a single table
  liftM mconcat (skipMany blankline *> many section <* eof) 


section :: TemplateParser VandelayTemplate 
section = 
      configSection
  <|> tableSection
  <|> subSection 
  <?> "section header (configuration, table, or substitutions)"







-- CONFIGURATION SECTION
configSection  :: TemplateParser VandelayTemplate
configLine     :: TemplateParser VandelayTemplate
configCommand  :: TemplateParser VandelayTemplate
configDataFile :: TemplateParser VandelayTemplate
configModels   :: TemplateParser VandelayTemplate
configTexfile  :: TemplateParser VandelayTemplate

configSection = do
  _   <- string "configuration:" <* eol 
  vts <- manyTillSectionHeader configLine 
  return $ mconcat vts 

configLine = 
      try ( blankline >> return blankVandelayTemplate)
  <|> space1 *> configCommand 

configCommand = 
      configDataFile 
  <|> configModels
  <|> configTexfile  
  <?> "Invalid configuration command. Valid commands are \"data:\", \"models:\", and \"tex:\"."
  

configDataFile = do 
  filePath <- string "data:" *> manyTillEol
  est      <- mapM (lift . readEstimatesEIO) . stripSplitCommas $ filePath
  return blankVandelayTemplate{estimates = est }


-- error "configDataFile not defined" -- basicCommand "data:" (\p -> blankConfiguration{datafile = Last . Just $ p})
configModels   = basicCommand "models:" (\p -> blankVandelayTemplate{desiredModels = Last . Just . stripSplitCommas$ p})
configTexfile  = basicCommand "tex:" (\p -> blankVandelayTemplate{texfile = Last . Just $ p})
 




-- TABLE SECTION
tableSection   :: TemplateParser VandelayTemplate
tableLine      :: TemplateParser (Maybe TableCommand)
tableStatement :: TemplateParser (Maybe TableCommand)

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
  <?> "table output statement (template, latex, code, index, format, name, scale, and surround)." 


-- DATA COMMAND
dataRow      :: TemplateParser (Maybe TableCommand)
dataCommands :: TemplateParser [Ordinal OutputRequest]
dataCommand  :: TemplateParser (Ordinal OutputRequest)

dataRow = do
  d <- dataCommands <* eol
  let or = mconcat $ defaultOutputRequest : sortExtractOrdinal d -- Sort these to ensure the StatLine comes first
  _ <- updateState (const or)
  return . Just $ Data or 

dataCommands = sepBy (spaces >> dataCommand) (char ';')


dataCommand = 
   getState >>= \lastOr -> 
      scale    <$>  (try (string "scale:")    *> many (noneOf ";\n"))
  <|> surround <$>  (try (string "surround:") *> many (noneOf ";\n"))
  <|> statLine 
        lastOr <$>  (     string "stat:"      *> spaces *> int ) 
  <|> index    <$>  (     string "index:"     *> spaces *> int ) 
  <|> name     <$>  (try (string "name:")     *> many (noneOf ";\n"))
  <|> code     <$>  (try (string "code:")     *> many (noneOf ";\n"))
  <|> format   <$>  (try (string "format:")   *> many (noneOf ";\n"))
    where
  scale t    = undefined
  statLine 
       lor i = Ordinal 1 lor{oName     = Last (Just ""), oItemIdx = Last (Just i)}
  name t     = Ordinal 2 blankOutputRequest{oName     = Last (Just t)}
  code t     = Ordinal 2 blankOutputRequest{oCoeffs   = Last .Just .stripSplitCommas $ t}
  index i    = Ordinal 2 blankOutputRequest{oItemIdx  = Last (Just i)}
  format t   = Ordinal 2 blankOutputRequest{oFormat   = Last (Just t)}
  surround t = Ordinal 2 blankOutputRequest{oSurround = Last (Just (getSurround t)) }  -- (preStr, postStr)}

getSurround t = let (preStr:postStr:other) = stripSplitCommas t
                in (preStr,postStr)






-- SUBSTITUTION SECTION
subSection     :: TemplateParser VandelayTemplate
subCommands    :: TemplateParser (Text, Text)
subDeclaration :: TemplateParser String

subSection = do 
  _    <- string "substitutions:" *> eol 
  cmds <- manyTillSectionHeader subCommands 
  return $ blankVandelayTemplate{substitutions = cmds}


subCommands = do 
  name <- subDeclaration

  -- Get position and track indent level
  pos  <- sourcePos 
  let indentlevel = sourceColumn pos - 1

  -- Substitutions
  s  <- manyTill anyChar eol 
  ss <- many (try (indentedOrBlankLine indentlevel)) 

  return (pack (stripStr name), pack (unlines (stripStr s : map stripStr ss)))


subDeclaration = 
  space1 *> many (noneOf ":\n\r") 
    <*  ( char ':' <?> "Colon needed to declare text source for substitutions.")










-- Parsec utility functions
basicCommand :: String 
                -> (String -> a) 
                -> TemplateParser a
basicCommand codestr fcn = 
  (fcn . stripStr) <$> (string codestr *> manyTillEol)


space1    = space >> spaces

eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"

sectionHeadOrEof :: TemplateParser String
sectionHeadOrEof =
      try (letter >> return "")
  <|> (eof >> return "")
  <?> "Expecting section definition or end of file."

manyTillEol             = manyTill anyChar eol 
manyTillSectionHeader x = manyTill x (lookAhead sectionHeadOrEof)


-- Taken from http://stackoverflow.com/questions/27469281/get-current-position-in-parsed-source-using-parsec
sourcePos :: TemplateParser SourcePos
sourcePos = statePos `liftM` getParserState

-- parseInt :: TemplateParser SourcePos 
-- parseInt = do
--   numStr <- many1 digit
--   sourcePos

-- Text utility functions
stripStr :: String -> String
stripStr = unpack . T.strip . pack 

int :: TemplateParser Int
int = read <$> (spaces >> many digit) 




blankline = many (char ' ')  *> eol *> return ""

indentedLine :: Int -- Indent level
             -> TemplateParser String
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


















