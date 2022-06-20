module Vandelay.DSL.Core.Text
  ( unwordEnglishList

  , joinAmps

  , commaPrintf

  , T.strip
  , T'.splitOn
  , surroundText
  , changeAllZeros

  ) where

-- import           Control.Monad.Error.Class
-- import           Text.Parsec               hiding (many, optional, (<|>))
import           Data.Char                 (isDigit)
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
  -- where nss = impureNonNull ss


commaPrintf ∷ String -- Format
            → Double -- Value
            → Text
commaPrintf fmt = commify . T.pack . printf fmt

commify ∷ Text → Text
commify s = revPrefix <> commiint <> fractional
    where
  (dirtyInt, fractional)  = T.break ( == '.' ) s
  (revInteger, revPrefix) = T.span isDigit . T.reverse $ dirtyInt
  commiint                = T.reverse . T.intercalate "," . T.chunksOf 3 . T.strip $ revInteger

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


-- stripSplitCommas ∷ Text → [Text]
-- stripSplitCommas = map T.strip . T'.splitOn ","

-- doSubstitution ∷ Text           -- Input
--                → [(Text, Text)] -- Substitutions
--                → Text           -- Transformed string
-- doSubstitution =
--   foldl' (\s (a,b) → T'.replace a b s)



-- type CommentParser = Parsec String ()

-- removeComments ∷ (MonadError ErrorMsg m) ⇒ Text → m Text
-- removeComments t =
--   case runParser commentedString () "Comment removal source" (T.unpack t) of
--     Left e  → throwError $ tshow e
--     Right r → return $ T.pack r

-- commentedString ∷ CommentParser String
-- commentedString = unlines <$> endBy commentLine eol

-- commentLine ∷ CommentParser String
-- commentLine = concat <$> (many escapedChar <* optional ( char '#' <* many (noneOf "\n\r")) )

-- escapedChar ∷ CommentParser String
-- escapedChar = ((:) <$> (char '\\' *> anyChar) <*> return [])
--            <|>((:[]) <$> noneOf "#\n\r")


-- commentChar ∷ CommentParser Char
-- commentChar = char '#'

-- eol ∷ Parsec String () String
-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"

