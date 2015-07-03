{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Init
  ( initTemplate
  , SortOptions(..)
  , SourceFileReferences(..)
  ) where

import App.Vandelay.Core
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWS
import Data.List
import System.FilePath
import qualified Data.Map as M
import qualified Data.Text as T

liftEIO :: a -> EIO String a
liftEIO = return 

liftEIOString :: String -> EIO String String
liftEIOString = return


-- Initialize template
initTemplate :: [String]       -- ^ Estimation results filepath
             -> Maybe String -- ^ Optional output file (stdout if nothing)
             -> SortOptions 
             -> SourceFileReferences -- ^ Use abbreviated model names
             -> EIO String () -- ^ Error message or ()
initTemplate estPaths textOutFile sos ab = do
  estFile    <- mapM safeReadFile estPaths
  (_,_,text) <- runRWST (writeConf >> writeTable >> writeSub ) (InitSetup estPaths estFile sos ab) ()
  
  safeWriteFile textOutFile text 





-- -- | Internal data types
type InitMonad   = RWST InitSetup String () (EIO String)
data InitSetup   = InitSetup 
  { dataFilePaths        :: [FilePath]
  , dataFileContents     :: [FileContent]
  , sortOptions          :: SortOptions
  , sourceFileReferences :: SourceFileReferences
  }

type FileContent = String

data SortOptions = SortOptions 
    { -- | Output models in order of appearance in estimates file if false
      sortModels :: Bool 
      -- | Output variables in order of appearance in estimates file  if false
    , sortVars   :: Bool
    }
    deriving (Show)

data SourceFileReferences = 
    NoSFR
  | FullPath
  | Abbreviation
  deriving (Show, Read, Eq)
  

-- | Write the template's configuration section
writeConf :: InitMonad () 
writeConf = do
  dfp <- askPath
  let   texFile  = replaceExtension baseFile ".tex"
        baseFile = takeFileName . head $ dfp

  tellLn   "configuration:"
  writeDataFiles
  tellLn   "  models: # List models separated by columns"
  tellLn $ "  tex:    " ++ texFile
  tellLn "" 
  tellLn "  # Models are:" 
  writeModels
  tellLn "" 
  

-- | Write the template's table section
writeTable :: InitMonad ()
writeTable = do
  tellLn "table:"
  tellLn "  template: header.tex # Subject to substitutions"
  tellLn "  name: Print Name; code: coded_name; index: 0; surround: (,); format: %03.2f # Variable output. Only code is required"
  tellLn "  latex: \\addlinespace # Source latex code - do not add end lines "
  tellLn "" 
  tellLn "  # Variables are:" 
  writeVars
  tellLn "" 

-- | Write the template's substitution section
writeSub :: InitMonad ()
writeSub = do 
  tellLn "substitutions:"
  tellLn "  CAPTION: Insert caption text which may"
  tellLn "           extends onto another line."





-- | Write the datafiles
writeDataFiles :: InitMonad ()
writeDataFiles = do
  paths <- askPath
  refs  <- askFileReferences
  sfr   <- asks sourceFileReferences

  if sfr /= Abbreviation then tellLn . indent . dataStatement . intercalate  ", " =<< askPath 
                         else let out   = map (indent . dataStatement *** refStatement) $ zip paths refs 
                              in  mapM_ tellLnWithDataSource =<< lengthenItemRefTuple out



-- | Write the models and variables 
writeModels :: InitMonad ()
writeVars   :: InitMonad ()

writeModels = writeItem askSortModels modelReferenceTuple
writeVars   = writeItem askSortVars   varReferenceTuple


writeItem :: InitMonad Bool                            -- | Sorting asker
          -> (String -> String -> [(String, [String])])-- | Item-reference tupling function 
          -> InitMonad ()
writeItem askSort tupler = do
  contents <- askContents 
  refs     <- askFileReferences
  sortQ    <- askSort

  let sorter    = if sortQ then id else sort
      vts       = concat . zipWith tupler
                  refs $ contents 
      vs        = sorter . M.toList . M.fromListWith (++) $ reverse vts  --- [(Variable String, [Data Source])]
      maxVarLen = maximum . map (length . fst) $ vs

      out   =  map ( indentAndComment *** dataSourceStatement ) vs 
  mapM_ tellLnWithDataSource =<< lengthenItemRefTuple out





-- | Model and variable from content
modelsFromContent :: String   -- | Content
                  -> [String] -- | Model
modelsFromContent =  
  stripFilter . T.splitOn tab . head . T.lines . pack


varsFromContent :: String   -- | Content
                -> [String] -- | [(Variable, Ref)]
varsFromContent =  
  stripFilter . map (head . T.splitOn tab) . T.lines . pack


itemReferenceTuple :: (String -> [String]) -- | Item Extraction function
                   -> String -- | Ref
                   -> String -- | Content
                   -> [(String, [String])] -- | [(Item, [Ref])]
itemReferenceTuple f r c = zip (f c) $ repeat [r]


varReferenceTuple   = itemReferenceTuple varsFromContent
modelReferenceTuple = itemReferenceTuple modelsFromContent

-- varReferenceTuple :: String -- | Ref
--                    -> String -- | Content
--                    -> [(String, [String])] -- | [(Variable, [Ref])]
-- varReferenceTuple r c = zip (varsFromContent c) $ repeat [r]


-- modelReferenceTuple :: String
--                     -> String               -- | Content
--                    -> [(String, [String])] -- | [(Variable, [Ref])]
-- modelReferenceTuple r c = zip (modelsFromContent c) $ repeat [r]







-- | Reader ask utility functions

askPath           :: InitMonad [FilePath]
askContents       :: InitMonad [FileContent]
askContentsRefs   :: InitMonad [(FileContent, String)]
askSortOptions    :: InitMonad SortOptions
askSortModels     :: InitMonad Bool
askSortVars       :: InitMonad Bool
askFileReferences :: InitMonad [String]

askPath           = asks dataFilePaths 
askContents       = asks dataFileContents
askContentsRefs   = zip <$> askContents <*> askFileReferences
askSortOptions    = asks sortOptions 
askSortModels     = liftM sortModels askSortOptions
askSortVars       = liftM sortVars   askSortOptions
askFileReferences = do
  a <- asks sourceFileReferences
  case a of 
    Abbreviation -> return abbreviations 
    otherwise    -> asks dataFilePaths









-- | Text utility functions
tellLn s = tell $ s ++ "\n"
tellLnWithDataSource (s,ds) = do
  b <- asks sourceFileReferences
  if b /= NoSFR then tellLn (s ++ ds) else tellLn s

stripFilter:: [Text] -> [String] -- Remove spaces, drop blanks
stripFilter = map unpack . filter (not . T.null) . map T.strip  

tab = "\t"

indent:: String -> String
indent = (++) "  "

indentAndComment :: String -> String
indentAndComment = indent . (++) "# " 

abbreviations = map (\c -> "(" ++ c : ")") ['A'..'Z']

extendString :: Int -> String -> String
extendString i s | length s > i = s
                 | otherwise    = s ++ replicate (i - length s) ' ' 

lengthenItemRefTuple :: [(String, String)] 
                     -> InitMonad [(String,String)]
lengthenItemRefTuple ts = do
  sfr <- asks sourceFileReferences
  if sfr /= NoSFR then let maxlength = maximum . map (length . fst) $ ts
                       in  return $ map (first (extendString maxlength)) ts
                       else return ts

-- | Statement creation
dataStatement :: String -> String
dataStatement = (++) "data: "

dataSourceStatement :: [String] -> String
dataSourceStatement rs = unwords [" # In", unwordEnglishList rs]

refStatement :: String -> String
refStatement s = unwords [" #", s]
