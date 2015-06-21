{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Init 
  ( initTemplate
  , SortOptions(..)
  ) where

import App.Vandelay.IO
import App.Vandelay.Types (EIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class 
import Control.Monad.Trans.RWS
-- import Control.Monad.Trans.Writer
import Data.List
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import System.FilePath



data SortOptions =  
    SortOptions { nosortmodels :: Bool
                , nosortvars   :: Bool
                }
    deriving (Show)


-- Initialize template
initTemplate :: String       -- Estimation results file
             -> Maybe String -- Output File
             -> SortOptions 
             -> EIO String () -- String, Handle)
initTemplate estPath textOutFile sos = do
  estFile <- safeReadFile estPath
  (_,_,text) <- runRWST (writeConf >> writeTable >> writeSub ) (estPath, estFile, sos) ()
  
  safeWriteFile textOutFile text 



-- Internal data types

type InitMonad        = RWST Setup String () (EIO String)
type Setup            = (DataFilePath, DataFileContents, SortOptions)
type DataFilePath     = String
type DataFileContents = String


askPath :: InitMonad DataFilePath
askPath = asks fst -- liftM fst ask
  where fst (a,_,_) = a

askContents :: InitMonad DataFileContents
askContents = liftM snd ask
  where snd (_,a,_) = a


askNoSortOptions :: InitMonad SortOptions
askNoSortOptions = liftM trd ask
  where trd (_,_,a) = a

askNoSortModels = liftM nosortmodels askNoSortOptions
askNoSortVars   = liftM nosortvars   askNoSortOptions

-- Template initialization printing functions 
writeConf :: InitMonad () 
writeConf = do
  dfp <- askPath
  let   texFile  = replaceExtension baseFile ".tex"
        baseFile = takeFileName dfp

  tellLn $ "configuration:"
  tellLn $ "  data:   " ++ dfp 
  tellLn $ "  models: # List models separated by columns"
  tellLn $ "  tex:    " ++ texFile

  tellLn $ "" 
  tellLn $ "  # Models are:" 
  writeModels
  tellLn $ "" 
  

writeTable :: InitMonad ()
writeTable = do
  tellLn $ "table:"
  tellLn $ "  template: header.tex # Subject to substitutions"
  tellLn $ "  name: Print Name; code: coded_name; index: 0; surround: (,); format: %03.2f # Variable output. Only code is required"
  tellLn $ "  latex: \\addlinespace # Source latex code - do not add end lines "

  tellLn $ "" 
  tellLn $ "  # Variables are:" 
  writeVars
  tellLn $ "" 

writeSub :: InitMonad ()
writeSub = do 
  tellLn $ "substitutions:"
  tellLn $ "  CAPTION: Insert caption text which may"
  tellLn $ "           extends onto another line."



-- Write models and variables 
writeVars :: InitMonad ()
writeVars = do 
  contents <- askContents 
  nosort   <- askNoSortVars

  let sorter = case nosort of
                False -> id
                True  -> sort

  let vs = sorter . stripFilter . map (head . T.splitOn tab) . T.lines . pack $ contents
  mapM_ (tellLn . tabAndComment) vs

writeModels :: InitMonad ()
writeModels = do
  contents <- askContents 
  nosort   <- askNoSortModels

  let sorter = case nosort of
                False -> id
                True  -> sort

  let ms = sorter . stripFilter . T.splitOn tab . head . T.lines . pack $ contents 
  mapM_ (tellLn . tabAndComment) ms






stripFilter:: [Text] -> [String] -- Remove spaces, drop blanks
stripFilter = map unpack . filter (not . T.null) . map T.strip  

tab :: Text
tab = "\t"

tabAndComment :: String -> String
tabAndComment s = "  # " ++ s


tellLn s = tell $ s ++ "\n"


