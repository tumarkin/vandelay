module Vandelay.DSL.Core.Text (
    unwordEnglishList,
    joinAmps,
    tprintF,
    surroundText,
    changeAllZeros,
) where

import Data.NonNull
import RIO
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Text.Printf

unwordEnglishList ∷ [Text] → Text
unwordEnglishList [] = ""
unwordEnglishList [s1] = s1
unwordEnglishList [s1, s2] = s1 <> " and " <> s2
unwordEnglishList ss = T.intercalate ", " (L'.init ss) <> ", and " <> L'.last ss

tprintF
    ∷ String -- Format
    → Double -- Value
    → Text
tprintF fmt = T.pack . printf fmt

joinAmps ∷ [Text] → Text
joinAmps = T.intercalate " & "

surroundText ∷ (Text, Text) → Text → Text
surroundText (prefix, postfix) s = prefix <> s <> postfix

-- | Modify 0.000 to $<$0.001 recognizing the number of decimal places
changeAllZeros
    -- :: Bool
    ∷ Text
    → Text
    → Text
-- changeAllZeros False t = t
changeAllZeros lessThan s =
    if T.any (`elem` ['1' .. '9']) s
        then s
        else fixZero s
  where
    fixZero ∷ Text → Text
    fixZero _s =
       lessThan <> init s <> "1"
      where
        s = fromMaybe (error "fixZero on empty string") (fromNullable . T.strip $ _s)
