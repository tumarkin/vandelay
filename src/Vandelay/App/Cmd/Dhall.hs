{-# LANGUAGE TemplateHaskell #-}
module Vandelay.App.Cmd.Dhall
  ( installLibrary
  ) where

import           Data.FileEmbed
import           RIO
import           RIO.Directory
import           RIO.FilePath

installLibrary
    ∷ FilePath
    → RIO env ()
installLibrary dir = do
    createDirectoryIfMissing True dir
    mapM_ (\(fp, bs) -> writeFileBinary (dir </> fp) bs) dhallFiles

dhallFiles ∷ [(FilePath, ByteString)]
dhallFiles = $(embedDir "dhall")
