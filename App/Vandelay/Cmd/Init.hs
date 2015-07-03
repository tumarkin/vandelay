{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Init 
  ( initTemplate
  , SortOptions(..)
  ) where

import App.Vandelay.Core
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class 
import Control.Monad.Trans.RWS
-- import Control.Monad.Trans.Writer
import Data.List
import qualified Data.Text as T
-- import Data.Text (Text, pack, unpack)
import System.FilePath



-- Initialize template
initTemplate :: String       -- ^ Estimation results filepath
             -> Maybe String -- ^ Optional output file (stdout if nothing)
             -> SortOptions 
             -> EIO String () -- ^ Error message or ()
initTemplate estPath textOutFile sos = do
  estFile <- safeReadFile estPath
  (_,_,text) <- runRWST (writeConf >> writeTable >> writeSub ) (estPath, estFile, sos) ()
  
  safeWriteFile textOutFile text 


data SortOptions =  
  SortOptions 
    { -- | Output models in order of appearance in estimates file if true
      nosortmodels :: Bool 
      -- | Output variables in order of appearance in estimates file  if true
    , nosortvars   :: Bool
    }
    deriving (Show)



-- | Internal data types
type InitMonad        = RWST Setup String () (EIO String)
type Setup            = (DataFilePath, DataFileContents, SortOptions)
type DataFilePath     = String
type DataFileContents = String


askPath          :: InitMonad DataFilePath
askContents      :: InitMonad DataFileContents
askNoSortOptions :: InitMonad SortOptions
askNoSortModels  :: InitMonad Bool
askNoSortVars    :: InitMonad Bool

askPath          = asks fst'
askContents      = asks snd'
askNoSortOptions = asks trd'
askNoSortModels  = liftM nosortmodels askNoSortOptions
askNoSortVars    = liftM nosortvars   askNoSortOptions

fst' (a,_,_) = a
snd' (_,a,_) = a
trd' (_,_,a) = a

-- | Write the template's configuration section
writeConf :: InitMonad () 
writeConf = do
  dfp <- askPath
  let   texFile  = replaceExtension baseFile ".tex"
        baseFile = takeFileName dfp

  tellLn   "configuration:"
  tellLn $ "  data:   " ++ dfp 
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



-- | Write the variables 
writeVars :: InitMonad ()
writeVars = do 
  contents <- askContents 
  nosort   <- askNoSortVars

  let sorter = if nosort then sort else id
      vs     = sorter . stripFilter . map (head . T.splitOn tab) . T.lines . pack $ contents
  mapM_ (tellLn . indentAndComment) vs

-- | Write the models 
writeModels :: InitMonad ()
writeModels = do
  contents <- askContents 
  nosort   <- askNoSortModels

  let sorter = if nosort then sort else id
      ms    = sorter . stripFilter . T.splitOn tab . head . T.lines . pack $ contents 
  mapM_ (tellLn . indentAndComment) ms





-- | Text utility functions
stripFilter:: [Text] -> [String] -- Remove spaces, drop blanks
stripFilter = map unpack . filter (not . T.null) . map T.strip  

tab :: Text
tab = "\t"

indentAndComment :: String -> String
indentAndComment s = "  # " ++ s

tellLn s = tell $ s ++ "\n"

