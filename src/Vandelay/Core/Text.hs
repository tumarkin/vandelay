module Vandelay.Core.Text
  ( unwordEnglishList

  , joinAmps

  , commaPrintf

  , stripSplitCommas
  , T.strip
  , T.splitOn

  , doSubstitution

  , removeCommentsEIO
  , removeComments

  ) where

import           Data.Char                 (isDigit)
import qualified Data.Text                 as T
import           Text.Parsec               hiding (many, optional, (<|>))
import           Text.Printf

import           Vandelay.Core.Modules hiding (try)
-- import           Vandelay.Core.Types


unwordEnglishList ∷ [Text] → Text
unwordEnglishList []      = ""
unwordEnglishList [s1]    = s1
unwordEnglishList [s1,s2] = s1 ++ " and " ++ s2
unwordEnglishList ss      = intercalate ", " (init nss) ++ ", and " ++ last nss
  where nss = impureNonNull ss


commaPrintf ∷ String -- Format
            → Double -- Value
            → Text
commaPrintf fmt = commify . pack . printf fmt

commify ∷ Text → Text
commify s = revPrefix ++ commiint ++ fractional
    where
  (dirtyInt, fractional)  = break ( == '.' ) s
  (revInteger, revPrefix) = span isDigit . reverse $ dirtyInt
  commiint                = reverse . intercalate "," . T.chunksOf 3 . T.strip $ revInteger

joinAmps ∷ [Text] → Text
joinAmps = intercalate " & "


stripSplitCommas ∷ Text → [Text]
stripSplitCommas = map T.strip . T.splitOn ","

doSubstitution ∷ Text           -- Input
               → [(Text, Text)] -- Substitutions
               → Text           -- Transformed string
doSubstitution =
  foldl' (\s (a,b) → T.replace a b s)



type CommentParser = Parsec String ()

removeCommentsEIO ∷ Text → EIO ErrorMsg Text
removeCommentsEIO t =
  case removeComments t of
    Left e  → throwError e
    Right r → return r

removeComments ∷ Text → Either ErrorMsg Text
removeComments t =
  case runParser commentedString () "Comment removal source" (unpack t) of
    Left  e → Left $ tshow e
    Right s → Right $ pack s

commentedString ∷ CommentParser String
commentedString = unlines <$> endBy commentLine eol

commentLine ∷ CommentParser String
commentLine = concat <$> (many escapedChar <* optional ( char '#' <* many (noneOf "\n\r")) )

escapedChar ∷ CommentParser String
escapedChar = ((:) <$> (char '\\' *> anyChar) <*> return [])
           <|>((:[]) <$> noneOf "#\n\r")


commentChar ∷ CommentParser Char
commentChar = char '#'

eol ∷ CommentParser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

