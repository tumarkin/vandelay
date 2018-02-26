module Vandelay.Template.ParserT
  ( readTemplateEIO
  ) where

import qualified Data.Text               as T
import           Text.Parsec             hiding (many, optional, (<|>))

import           Vandelay.Core           hiding (try)
import           Vandelay.Shared.ParserT
import           Vandelay.Estimates


--- External interface
readTemplateEIO ∷ FilePath  -- File name
                 → EIO ErrorMsg VandelayTemplate
readTemplateEIO f =
  safeReadFileWithError f "Template file"
    >>= removeCommentsEIO
    >>= runParserT tablefile blankOutputRequest ("Template file: " ++ f) . unpack
    >>= \case
      Left e  → throwError $ tshow e
      Right r → return r



--- Parser
type TemplateParser    = ParsecT String UserState (EIO ErrorMsg)

type LastOutputRequest = OutputRequest
type UserState         = LastOutputRequest


tablefile ∷ TemplateParser VandelayTemplate
tablefile =
  mconcat <$> (skipMany blankline *> many section <* eof)


section ∷ TemplateParser VandelayTemplate
section =
      configSection
  <|> tableSection
  <|> subSection
  <?> "section header (configuration, table, or substitutions)"







-- CONFIGURATION SECTION
configSection  ∷ TemplateParser VandelayTemplate
configLine     ∷ TemplateParser VandelayTemplate
configCommand  ∷ TemplateParser VandelayTemplate
configDataFile ∷ TemplateParser VandelayTemplate
configModels   ∷ TemplateParser VandelayTemplate
configTexfile  ∷ TemplateParser VandelayTemplate

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
  estHMs   <- mapM (lift . readEstimatesEIO . unpack) . stripSplitCommas . pack $ filePath
  return blankVandelayTemplate{estimatesHM = mconcat estHMs }


configModels   = tBasicCommand
                  "models:"
                  (\p → blankVandelayTemplate{desiredModels = extractFilePath <$> stripSplitCommas p})
  -- where
extractFilePath ∷ Text → (Maybe FilePath, Text)
extractFilePath t = 
    if | null post → (Nothing, t)
       | otherwise → (Just $ unpack pre, tail . impureNonNull $ post)
  where
    (pre, post) = T.breakOn ":" t

configTexfile  = basicCommand
                  "tex:"
                  (\p → blankVandelayTemplate{texfile = Just p})





-- TABLE SECTION
tableSection   ∷ TemplateParser VandelayTemplate
tableLine      ∷ TemplateParser (Maybe TableCommand)
tableStatement ∷ TemplateParser (Maybe TableCommand)

tableSection = do
  _    <- string "table:" *> eol
  cmds <-  manyTillSectionHeader tableLine
  return $ blankVandelayTemplate{table = catMaybes cmds}


tableLine =
      try (blankline >> return Nothing)
  <|> space1 *> tableStatement


tableStatement =
      try (basicCommand "template:" (Just . Template))
  <|> try (tBasicCommand "latex:" (Just . Latex))
  <|> try dataRow
  <?> "table output statement (template, latex, code, index, format, name, scale, and surround)."


-- DATA COMMAND
dataRow      ∷ TemplateParser (Maybe TableCommand)
dataCommands ∷ TemplateParser [Prioritized (OutputRequest → OutputRequest)]
dataCommand  ∷ TemplateParser (Prioritized (OutputRequest → OutputRequest))

dataRow = do
  ds <- dataCommands <* eol

  -- Overwrite the old name
  let combinedF = foldl' (.) id (getPrioritizedItem <$> sort ds)
      or        = combinedF blankOutputRequest
  setState or
  return . Just $ Data or

dataCommands = sepBy (spaces >> dataCommand) (char ';')

dataCommand =
    getState >>= \lc → -- Last command
        Sec  . scale       <$>  (try (string "scale:")    *> spaces *> double)
    <|> Sec  . surround    <$>  (try (string "surround:") *> many (noneOf ";\n"))
    <|> Prim . statLine lc <$>  (     string "stat:"      *> spaces *> int )
    <|> Sec  . index       <$>  (     string "index:"     *> spaces *> int )
    <|> Sec  . name        <$>  (try (string "name:")     *> many (noneOf ";\n"))
    <|> Sec  . code        <$>  (try (string "code:")     *> many (noneOf ";\n"))
    <|> Sec  . format      <$>  (try (string "format:")   *> many (noneOf ";\n"))
    <|> Sec  . empty       <$>  (try (string "empty:")    *> many (noneOf ";\n"))
  where
    scale          = set oScale
    surround       = set oSurround . getSurround
    statLine lor i = const lor{_oItemIdx = i, _oName = ""}
    name           = set oName  . pack
    code           = set oCoeffs . stripSplitCommas . pack
    index          = set oItemIdx
    format         = set oFormat
    empty          = set oEmpty . pack

getSurround ∷ String → (Text, Text)
getSurround t = let (preStr:postStr:_) = stripSplitCommas $ pack t
                in  (preStr,postStr)

data Prioritized a
  = Prim a -- Primary
  | Sec  a -- Secondary
  deriving (Show)

instance Eq (Prioritized a) where
  (Prim _) == (Prim _) = True
  (Sec  _) == (Sec  _) = True
  _        == _        = False

instance Ord (Prioritized a) where
  (Sec  _) <= (Prim _) = False
  _        <= _        = True

getPrioritizedItem ∷ Prioritized a → a
getPrioritizedItem (Prim a) = a
getPrioritizedItem (Sec  a) = a







-- SUBSTITUTION SECTION
subSection     ∷ TemplateParser VandelayTemplate
subCommands    ∷ TemplateParser (Text, Text)
subDeclaration ∷ TemplateParser String

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

  return (pack (stripStr name), -- Strip the spaces around the name
          T.dropWhileEnd isCRLF . pack . unlines $ stripStr s : map stripStr ss -- Strip the spaces around each line and trim any trailing carriage returns
         )


subDeclaration =
  space1 *> many (noneOf ":\n\r")
    <*  ( char ':' <?> "Colon needed to declare text source for substitutions.")

isCRLF ∷ Char → Bool
isCRLF c = c `elem` asString "\n\r"





-- Parsec utility functions
basicCommand ∷ String
             → (String → a)
             → TemplateParser a
basicCommand codestr fcn =
  (fcn . stripStr) <$> (string codestr *> manyTillEol)

tBasicCommand ∷ String
             → (Text → a)
             → TemplateParser a
tBasicCommand codestr fcn =
  (fcn . strip . pack) <$> (string codestr *> manyTillEol)


space1    = space >> spaces

sectionHeadOrEof ∷ TemplateParser String
sectionHeadOrEof =
      try (letter >> return "")
  <|> (eof >> return "")
  <?> "Expecting section definition or end of file."

manyTillEol             = manyTill anyChar eol
manyTillSectionHeader x = manyTill x (lookAhead sectionHeadOrEof)


-- Taken from http://stackoverflow.com/questions/27469281/get-current-position-in-parsed-source-using-parsec
sourcePos ∷ TemplateParser SourcePos
sourcePos = statePos `liftM` getParserState

-- Text utility functions
stripStr ∷ String → String
stripStr = unpack . strip . pack





blankline = many (char ' ')  *> eol *> return ""

indentedLine ∷ Int -- Indent level
             → TemplateParser String
indentedLine i = count i (char ' ')  -- Use this instead of space to ensure it doesn't include EOL
  *> manyTill anyChar eol

indentedOrBlankLine i =
      try ( indentedLine i )
  <|> try blankline






