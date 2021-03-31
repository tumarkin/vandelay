module Vandelay.App.Template.ParserT
  ( readTemplate
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Data.Text                 as T
import           Text.Parsec               hiding (many, optional, (<|>))

import           Vandelay.DSL.Core         hiding (count, try)
import           Vandelay.DSL.Estimates



-- External interface
readTemplate ∷ (MonadError ErrorMsg m, MonadIO m)
             ⇒ FilePath  -- File name
             → m VandelayTemplate
readTemplate f =
  safeReadFileWithError f "Template file"
    >>= removeComments
    >>= runParserT tablefile blankOutputRequest ("Template file: " ++ f) . unpack
    >>= \case
      Left e  → throwError $ tshow e
      Right r → return r



--- Parser
type TemplateParser m  = ParsecT String UserState m
type LastOutputRequest = OutputRequest
type UserState         = LastOutputRequest


tablefile ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
tablefile =
  mconcat <$> (skipMany blankline *> many section <* eof)


section ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
section =
      configSection
  <|> tableSection
  <|> subSection
  <?> "section header (configuration, table, or substitutions)"







-- CONFIGURATION SECTION
configSection  ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
configLine     ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
configCommand  ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
configDataFile ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
configModels   ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate

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
  <?> "Invalid configuration command. Valid commands are \"data:\" and \"models:\"."


configDataFile = do
  filePath <- string "data:" *> manyTillEol
  estHMs   <- mapM (lift . readEstimates . unpack) . stripSplitCommas . pack $ filePath
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



-- TABLE SECTION
tableSection   ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
tableLine      ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m (Maybe TableCommand)
tableStatement ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m (Maybe TableCommand)

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
  <?> "table output statement (template, latex, code, index, format, name, scale, surround, zeroReformat)."


-- DATA COMMAND
dataRow      ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m (Maybe TableCommand)
dataCommands ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m [Prioritized (OutputRequest → OutputRequest)]
dataCommand  ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m (Prioritized (OutputRequest → OutputRequest))

dataRow = do
  ds <- dataCommands <* eol

  -- Overwrite the old name
  let combinedF = foldr (.) id (getPrioritizedItem <$> sort ds)
      or        = combinedF blankOutputRequest
  setState or
  return . Just $ Data or

dataCommands = sepBy (spaces >> dataCommand) (char ';')

dataCommand =
    getState >>= \lc → -- Last command
        Sec  . scale        <$>  (try (string "scale:")    *> spaces *> double)
    <|> Sec  . surround     <$>  (try (string "surround:") *> many (noneOf ";\n"))
    <|> Prim . statLine lc  <$>  (     string "stat:"      *> spaces *> int )
    <|> Sec  . index        <$>  (     string "index:"     *> spaces *> int )
    <|> Sec  . name         <$>  (try (string "name:")     *> many (noneOf ";\n"))
    <|> Sec  . code         <$>  (try (string "code:")     *> many (noneOf ";\n"))
    <|> Sec  . format       <$>  (try (string "format:")   *> many (noneOf ";\n"))
    <|> Sec  . empty        <$>  (try (string "empty:")    *> many (noneOf ";\n"))
    <|> Sec  . zeroReformat <$>  (try (string "zeroReformat:") *> spaces *> boolP)
  where
    scale          = set oScale
    surround       = set oSurround . getSurround
    statLine lor i = const lor{_oItemIdx = i, _oName = ""}
    name           = set oName  . pack
    code           = set oCoeffs . stripSplitCommas . pack
    index          = set oItemIdx
    format         = set oFormat
    empty          = set oEmpty . pack
    zeroReformat   = set oModifyZero


getSurround ∷ String → (Text, Text)
getSurround t = let (preStr:postStr:_) = stripSplitCommas $ pack t
                in  (preStr,postStr)

-- | A way to prioritize actions. In this case, we want the primary action to
-- go first, so it should occur last in a list of actions when foldling with
-- (.). This is implemented in the Ord Instance.
data Prioritized a
  = Prim a -- Primary
  | Sec  a -- Secondary
  deriving (Show)

instance Eq (Prioritized a) where
  (Prim _) == (Prim _) = True
  (Sec  _) == (Sec  _) = True
  _        == _        = False

instance Ord (Prioritized a) where
  (Prim  _) <= (Sec _) = False
  _         <= _        = True

getPrioritizedItem ∷ Prioritized a → a
getPrioritizedItem (Prim a) = a
getPrioritizedItem (Sec  a) = a







-- SUBSTITUTION SECTION
subSection     ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m VandelayTemplate
subCommands    ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m (Text, Text)
subDeclaration ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m String

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
basicCommand ∷ (MonadError ErrorMsg m, MonadIO m)
             ⇒  String
             → (String → a)
             → TemplateParser m a
basicCommand codestr fcn =
  (fcn . stripStr) <$> (string codestr *> manyTillEol)

tBasicCommand ∷ (MonadError ErrorMsg m, MonadIO m)
              ⇒  String
              → (Text → a)
              → TemplateParser m a
tBasicCommand codestr fcn =
  (fcn . strip . pack) <$> (string codestr *> manyTillEol)


space1 ∷ (MonadError ErrorMsg m, MonadIO m)
       ⇒ TemplateParser m ()
space1 = space >> spaces

sectionHeadOrEof ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m String
sectionHeadOrEof =
      try (letter >> return "")
  <|> (eof >> return "")
  <?> "Expecting section definition or end of file."

manyTillEol ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m String
manyTillEol = manyTill anyChar eol

manyTillSectionHeader ∷ (MonadError ErrorMsg m, MonadIO m)
                      ⇒  TemplateParser m a
                      → TemplateParser m [a]
manyTillSectionHeader x = manyTill x (lookAhead sectionHeadOrEof)


-- Taken from http://stackoverflow.com/questions/27469281/get-current-position-in-parsed-source-using-parsec
sourcePos ∷ (MonadError ErrorMsg m, MonadIO m) ⇒  TemplateParser m SourcePos
sourcePos = statePos `liftM` getParserState

-- Text utility functions
stripStr ∷ String → String
stripStr = unpack . strip . pack




blankline ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ TemplateParser m String
blankline = many (char ' ')  *> eol *> return ""

indentedLine ∷ (MonadError ErrorMsg m, MonadIO m)
             ⇒  Int -- Indent level
             → TemplateParser m String
indentedLine i = count i (char ' ')  -- Use this instead of space to ensure it doesn't include EOL
  *> manyTill anyChar eol

indentedOrBlankLine ∷ (MonadError ErrorMsg m, MonadIO m)
             ⇒  Int -- Indent level
             → TemplateParser m String
indentedOrBlankLine i =
      try ( indentedLine i )
  <|> try blankline






