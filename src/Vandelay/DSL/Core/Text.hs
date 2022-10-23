module Vandelay.DSL.Core.Text
  ( unwordEnglishList

  , joinAmps
  , packPrintf
  , T.strip
  , T'.splitOn
  , surroundText
  , changeAllZeros

  ) where

import           Data.NonNull
import qualified RIO.List.Partial          as L'
import qualified RIO.Text                  as T
import qualified RIO.Text.Partial          as T'
import           Text.Printf
import           Vandelay.DSL.Core.Modules hiding (try)


unwordEnglishList ∷ [Text] → Text
unwordEnglishList []      = ""
unwordEnglishList [s1]    = s1
unwordEnglishList [s1,s2] = s1 <> " and " <> s2
unwordEnglishList ss      = (T.intercalate ", " (L'.init ss)) <> ", and " <> L'.last ss

packPrintf ∷ String -- Format
           → Double -- Value
           → Text
packPrintf fmt = T.pack . printf fmt

joinAmps ∷ [Text] → Text
joinAmps = T.intercalate " & "

surroundText ∷ (Text, Text) → Text → Text
surroundText (prefix, postfix) s = prefix <> s <> postfix

changeAllZeros ∷ Bool  -- ^ Modify all zeros to < (e.g. 0.000 to <0.001)
               → Text
               → Text
changeAllZeros False t = t
changeAllZeros True s = if T.any (`elem` ['1'..'9']) s then s
                     else fixZero s

-- | Change 0.000 to $<$0.001 recognizing the number of decimal places
fixZero ∷ Text → Text
fixZero _s =
    "$<$" <> init s <> "1"
  where
    s = fromMaybe (error "fixZero on empty string") (fromNullable . T.strip $ _s)


