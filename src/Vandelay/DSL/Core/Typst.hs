module Vandelay.DSL.Core.Typst (
    typify,
) where

import RIO
import qualified RIO.Text as T

import Vandelay.DSL.Core.Text
import Vandelay.DSL.Core.Types

typify ∷ OutputRequest → DataItem → Text
typify _ (StrData t) = brackets t
typify or BlankData = brackets $ fromMaybe "" or.formatSpec.empty
-- texify or (ValData v s) = surroundText st (changeAllZeros caz (commaPrintf fmt (scale * v))) <> makeStars s
typify or (ValData v s) = 
    let formattedValue = zeroFormatter $ tprintF fmt (scale * v) 
        stars = makeStars s
    in brackets . surroundText st $ formattedValue <> stars
  where
    -- caz   = or.formatSpec.modifyZero
    st = fromMaybe ("", "") $ or.formatSpec.surround
    fmt = T.unpack $ or.formatSpec.format
    scale = or.formatSpec.scale

    zeroFormatter = bool id (changeAllZeros "<") or.formatSpec.modifyZero

makeStars ∷ Int → Text
makeStars i = T.replicate i "*"

brackets ∷ Text → Text
brackets = surroundText ("[", "]")

-- quote ∷ Text → Text
-- quote = surroundText ("\"", "\"")

-- parens ∷ Text → Text
-- parens = surroundText ("(", ")")


-- columnFunc :: String -> Text -> Text
-- columnFunc fmt  t = brackets $ "#cad" <> parens args
--   where
--     args = quote t <> ", fmt: " <> (quote $ tprintF fmt 0.0)

