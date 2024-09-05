module Vandelay.DSL.Core.Tex (
    texify,
) where

import RIO
import qualified RIO.Text as T

import Vandelay.DSL.Core.Text
import Vandelay.DSL.Core.Types

texify ∷ OutputRequest → DataItem → Text
texify _ (StrData t) = "{" <> t <> "}"
texify or BlankData = fromMaybe "" $ or.formatSpec.empty
-- texify or (ValData v s) = surroundText st (changeAllZeros caz (commaPrintf fmt (scale * v))) <> makeStars s
texify or (ValData v s) = surroundText st (zeroFormatter (tprintF fmt (scale * v))) <> makeStars s
  where
    -- caz   = or.formatSpec.modifyZero
    st = fromMaybe ("", "") $ or.formatSpec.surround
    fmt = T.unpack $ or.formatSpec.format
    scale = or.formatSpec.scale

    zeroFormatter = bool id changeAllZeros or.formatSpec.modifyZero

makeStars ∷ Int → Text
makeStars i
    | i == 0 = ""
    | otherwise = "\\sym{" <> T.replicate i "*" <> "}"
