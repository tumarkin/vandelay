{-# LANGUAGE OverloadedStrings #-}

module App.Vandelay.Cmd.Make 
  ( makeTable
  ) where


import App.Vandelay.Estimates
import App.Vandelay.IO
import App.Vandelay.Text 
import App.Vandelay.Types
import App.Vandelay.Template
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T



makeTable :: String -> EIO String () -- String, Handle)
makeTable template 
    = readTemplateEIO template
  >>= \vt     -> return (configuration vt)
  >>= \config -> hoistEither (safeGetDatafile config)
  >>= \df     -> readEstimatesEIO (unpack df)
  >>= \est    -> createOutput vt est
  >>= \res    -> safeWriteFile (getLast . texfile $ config) (pack . unlines $ res)
  >>  return ()


--- Internal functions 
createOutput :: VandelayTemplate 
             -> Estimates 
             -> EitherT String IO [String]
createOutput vt est = 
  mapM (doTableCommand vt est) cmds
    where 
  config = configuration vt
  cmds   = table vt



doTableCommand :: VandelayTemplate 
               -> Estimates 
               -> TableCommand 
               -> EitherT String IO String

doTableCommand vt est (Latex l)          = right $ unpack l ++ "\\\\"
doTableCommand vt est (Template t)       = safeReadFile (unpack t)
                                       >>= return . doSubs vt
doTableCommand vt est (Data   or)        = outputRowEIO est dms or
                                       >>= \t -> return $ t ++ "\\\\"
    where
  config = configuration vt
  dms    = stripSplitCommas . fromJust . getLast . desiredModels $ config




doSubs :: VandelayTemplate -> String -> String
doSubs vt s =
  unpack $ foldl (\s (a,b) -> T.replace a b s) txt subs
    where 
  subs = substitutions vt
  txt  = pack s

