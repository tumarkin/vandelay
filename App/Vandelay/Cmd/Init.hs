{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Init 
  ( initTemplate
  , SortOptions(..)
  ) where

import App.Vandelay.IO
import App.Vandelay.Types (EIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.List
import qualified Data.Text as T
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
initTemplate estfile outfile sos = do
  
  (ms, vs) <- modelsVariables estfile

  let textOutFile = case outfile of 
                        Nothing -> Nothing
                        Just s  -> Just $ T.pack s

  let sm = case nosortmodels sos of
              False -> sort
              True  -> id 
      sv = case nosortvars sos of
              False -> sort
              True  -> id

      modelTxt = map tabAndComment . sm $ ms
      varsTxt  = map tabAndComment . sv $ vs

      text = T.unlines . concat . intersperse ["\n"] $ 
        [ setupTxt estfile
        , modelTxt
        , tableTxt 
        , varsTxt 
        , subTxt
        ]


  safeWriteFile textOutFile text 




-- Basic configuration
setupTxt fp = [ "configuration:"
            , "  data:   " `T.append` (T.pack fp)
            , "  models: # List models separated by columns"
            , "  tex:    " `T.append` (T.pack texFile)
            ]
  where 
    texFile  = replaceExtension baseFile ".tex"
    baseFile = takeFileName fp

tableTxt  = [ "table:"
            , "  template: test/header.tex # Subsject to substitutions"
            , "  name: Print Name; code: log_age; index: 1; surround: (,); format: %03.2f # Variable output. Only code is required"
            , "  latex: \\addlinespace # Source latex code"
            ]
subTxt    = [ "substitutions:"
            , "  CAPTION: Insert caption text which may"
            , "           extends onto another line."
            ]



-- Get models and variables from a file
modelsVariables :: String -> EIO String ([T.Text],[T.Text])
modelsVariables s = 
         safeReadFile s 
    >>= return . T.pack
    >>= \ptxt -> return (models ptxt,variables ptxt)

models :: T.Text -> [T.Text]
models = stripFilter . T.splitOn tab . head . T.lines 

variables :: T.Text -> [T.Text]
variables = stripFilter . map (head . T.splitOn tab) . T.lines 


stripFilter:: [T.Text] -> [T.Text] -- Remove spaces, drop blanks, and sort
stripFilter = filter (not . T.null) . map T.strip  

tab :: T.Text
tab = "\t"

tabAndComment :: T.Text -> T.Text
tabAndComment s = "\t\t# " `T.append` s











