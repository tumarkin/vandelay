{-# LANGUAGE OverloadedStrings #-}
module Vandelay.App.Cmd.Init
  ( initTemplate
  , SortOptions(..)
  , SourceFileReferences(..)
  ) where

import           Control.Monad.Trans.RWS (RWST, runRWST, tell)
import           Data.NonNull
import           RIO.FilePath
import qualified RIO.List                as L
import qualified RIO.Map                 as M
import qualified RIO.Text                as T
import           Vandelay.DSL.Core

-- Initialize template
initTemplate ∷ [FilePath]                         -- ^ Estimation results filepaths
             → Maybe FilePath                     -- ^ Optional output file (stdout if nothing)
             → SortOptions
             → SourceFileReferences               -- ^ Use abbreviated model names
             → ExceptT ErrorMsg (RIO env) ()      -- ^ Error message or ()
initTemplate estPaths textOutFile sos sfr = do
  globs      <- globPaths estPaths
  estFile    <- mapM safeReadFile globs
  (_,_,text) <- runRWST (writeConf >> writeTable >> writeSub ) (InitSetup estPaths estFile sos sfr) ()

  unsafeWriteFile textOutFile text


-- -- | Internal data types
type InitMonad env = RWST InitSetup Text () (ExceptT ErrorMsg (RIO env))
data InitSetup   = InitSetup
  { dataFilePaths        ∷ [FilePath]
  , dataFileContents     ∷ [FileContent]
  , sortOptions          ∷ SortOptions
  , sourceFileReferences ∷ SourceFileReferences
  }

type FileContent = Text
type FileRefs    = Text

data SortOptions = SortOptions
    { -- | Output models in order of appearance in estimates file if false
      sortModels ∷ Bool
      -- | Output variables in order of appearance in estimates file  if false
    , sortVars   ∷ Bool
    }
    deriving (Show)

data SourceFileReferences =
    NoSFR
  | FullPath
  | Abbreviation
  deriving (Show, Read, Eq)


-- | Write the template's configuration section
writeConf ∷ InitMonad env ()
writeConf = do
  dfp <- askPath
  baseFile <- maybe (throwError "No file paths specified")
                    (return . takeFileName . head)
                    (fromNullable dfp)

  let   texFile  = replaceExtension baseFile ".tex"

  tellLn   "configuration:"
  writeDataFiles
  tellLn   "  models: # List models separated by columns as (FILENAME:)MODELNAME where FILENAME is optional"
  tellLn $ "  tex:    " <> T.pack texFile
  tellLn ""
  tellLn "  # Models are:"
  writeModels
  tellLn ""


-- | Write the template's table section
writeTable ∷ InitMonad env ()
writeTable = do
  tellLn "table:"
  tellLn "  template: header.tex # Subject to substitutions"
  tellLn "  name: Print Name; code: coded_name; index: 0; surround: (,); format: %03.2f; scale: 1.0; empty: -; zeroReformat: BOOL"
  tellLn "  latex: \\addlinespace # Source latex code - add end lines if required"
  tellLn ""
  tellLn "  # Note that specifying missing in the data row (beginning with name) will allow for a file to omit a variable. This differs from blank values in a file with a variable."
  tellLn ""
  tellLn "  # Variables are:"
  writeVars
  tellLn ""

-- | Write the template's substitution section
writeSub ∷ InitMonad env ()
writeSub = do
  tellLn "substitutions:"
  tellLn "  CAPTION: Insert caption text which may"
  tellLn "           extends onto another line."





-- | Write the datafiles
writeDataFiles ∷ InitMonad env ()
writeDataFiles = do
  paths <- map T.pack <$> askPath
  refs  <- askFileReferences
  sfr   <- asks sourceFileReferences

  if sfr /= Abbreviation
  then tellLn . indent . dataStatement . T.intercalate  ", " . map T.pack =<< askPath
  else let out   = map (indent . dataStatement *** refStatement) $ zip paths refs
       in  mapM_ tellLnWithDataSource =<< lengthenItemRefTuple out



-- | Write the models and variables
writeModels ∷ InitMonad env ()
writeVars   ∷ InitMonad env ()

writeModels = writeItem askSortModels modelReferenceTuple
writeVars   = writeItem askSortVars   varReferenceTuple


writeItem ∷ InitMonad env Bool               -- | Sorting asker
          → (Text → Text → [(Text, [Text])]) -- | Item-reference tupling function
          → InitMonad env ()
writeItem askSort tupler = do
  conrefs  <- askContentsRefs -- [(Content, Source File Reference)]
  sortQ    <- askSort

  let sorter ∷ Ord a ⇒ [a] → [a]
      sorter = if sortQ then id else L.sort
      its    = concatMap (uncurry tupler) conrefs                     -- [(Item, Source File Reference)]
      is     = sorter . M.toList . M.fromListWith (++) $ reverse its  -- [(Item, [Source File Reference])] -- Group by item
      out    = map (indentAndComment *** dataSourceStatement) is    -- Format each part of tuple using arrows
  mapM_ tellLnWithDataSource =<< lengthenItemRefTuple out





-- | Model and variable from content
modelsFromContent ∷ Text   -- | Content
                  → [Text] -- | Model
modelsFromContent t = 
  maybe []
        (stripFilter . splitOn tab . head)
        (fromNullable . T.lines $ t)


varsFromContent ∷ Text   -- | Content
                → [Text] -- | [(Variable, Ref)]
varsFromContent = 
  stripFilter . mapMaybe (fmap head . fromNullable . splitOn tab) . T.lines


itemReferenceTuple ∷ (Text → [Text]) -- | Item Extraction function
                   → Text -- | Content
                   → Text -- | Refs
                   → [(Text, [Text])] -- | [(Item, [Ref])]
itemReferenceTuple f c r = zip (f c) $ L.repeat [r]


varReferenceTuple   = itemReferenceTuple varsFromContent
modelReferenceTuple = itemReferenceTuple modelsFromContent

-- | Reader ask utility functions

askPath                 ∷ InitMonad env [FilePath]
askTPath                ∷ InitMonad env [Text]
askContents             ∷ InitMonad env [FileContent]
askContentsRefs         ∷ InitMonad env [(FileContent, FileRefs)]
askSortOptions          ∷ InitMonad env SortOptions
askSortModels           ∷ InitMonad env Bool
askSortVars             ∷ InitMonad env Bool
askSourceFileReferences ∷ InitMonad env SourceFileReferences
askFileReferences       ∷ InitMonad env [FileRefs]

askPath                 = asks dataFilePaths
askTPath                = map T.pack <$> askPath
askContents             = asks dataFileContents
askContentsRefs         = zip <$> askContents <*> askFileReferences
askSortOptions          = asks sortOptions
askSortModels           = sortModels <$> askSortOptions
askSortVars             = sortVars   <$> askSortOptions
askSourceFileReferences = asks sourceFileReferences
askFileReferences =
  askSourceFileReferences >>= \case
    Abbreviation → return abbreviations
    _            → map T.pack <$> asks dataFilePaths









-- | WriterT utility functions
tellLn s = tell $ s <> "\n"
tellLnWithDataSource (s,ds) = 
  askSourceFileReferences >>= \case
    NoSFR → tellLn s
    _     → tellLn (s <> ds)


-- | Statement creation
dataStatement ∷ Text → Text
dataStatement = (<>) "data: "

dataSourceStatement ∷ [Text] → Text
dataSourceStatement rs = T.unwords [" # In", unwordEnglishList rs]

refStatement ∷ Text → Text
refStatement s = T.unwords [" #", s]


-- | Text utility functions
stripFilter∷ [Text] → [Text] -- | Remove spaces, drop blanks
stripFilter = filter (not . T.null) . map T.strip

tab :: Text
tab = "\t"

indent∷ Text → Text
indent = (<>) "  "

indentAndComment ∷ Text → Text
indentAndComment = indent . (<>) "# "

abbreviations ∷ [Text]
abbreviations = map (\c → "(" <> T.singleton c <> ")") ['A'..'Z']

extendText ∷ Int → Text → Text
extendText i s | T.length s > i = s
               | otherwise    = s <> T.replicate (i - T.length s) (T.singleton ' ')

lengthenItemRefTuple ∷ [(Text, Text)]
                     → InitMonad env [(Text,Text)]
lengthenItemRefTuple ts =
  asks sourceFileReferences >>= \case
    NoSFR → return ts
    _     → let maxlength ∷ Int
                maxlength = maximum $ impureNonNull (0 : map getLength ts)

                getLength ∷ (Text, Text) → Int
  -- , unwordEnglishListString
                getLength (t, _) = T.length t
            in  return $ map (first (extendText maxlength)) ts

