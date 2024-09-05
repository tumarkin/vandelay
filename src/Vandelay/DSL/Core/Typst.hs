module Vandelay.DSL.Core.Typst (
    typify,
) where

import RIO
import qualified RIO.Text as T

import Vandelay.DSL.Core.Text
import Vandelay.DSL.Core.Types

typify ∷ OutputRequest → DataItem → Int → Text
typify _ (StrData t) _ = brackets t
typify or BlankData _ = brackets $ fromMaybe "" or.formatSpec.empty
-- texify or (ValData v s) = surroundText st (changeAllZeros caz (commaPrintf fmt (scale * v))) <> makeStars s
typify or (ValData v s) col = 
    let formattedValue = zeroFormatter $ tprintF fmt (scale * v) 
        stars = makeStars s
    in columnFunc col . surroundText st $ formattedValue <> stars
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

brackets ∷ Text → Text
brackets = surroundText ("[", "]")

quote ∷ Text → Text
quote = surroundText ("\"", "\"")


columnFunc :: Int -> Text -> Text
columnFunc i t = "#col" <> tshow i <> (brackets . quote $ t)
