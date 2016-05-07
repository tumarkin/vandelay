module App.Vandelay.Core.Text
  ( unwordEnglishList

  , joinAmps

  , commaPrintf

  , stripSplitCommas 
  , doSubstitution

  , removeCommentsEIO
  , removeComments

  -- Reexports from Text
  , Text
  , pack
  , unpack

  , lowercase


  ) where

import Data.Char
import Data.List.Split hiding (endBy)
import Data.Text (Text, pack, unpack)
import Text.Parsec hiding (many, (<|>), optional)
import Text.Printf
import qualified Data.Text as T

import App.Vandelay.Core.Types
import App.Vandelay.Core.Modules


unwordEnglishList :: [String] -> String
unwordEnglishList [s1]    = s1
unwordEnglishList [s1,s2] = s1 ++ " and " ++ s2
unwordEnglishList ss      = intercalate ", " (init ss) ++ ", and " ++ last ss


commaPrintf :: String -- Format
            -> Double -- Value
            -> String
commaPrintf fmt d = commify . printf fmt $ d

commify :: String -> String
commify s = revPrefix ++ commiint ++ fractional
    where
  (dirtyInt, fractional)  = break ( == '.' ) s
  (revInteger, revPrefix) = span isDigit . reverse $ dirtyInt
  commiint                = reverse . intercalate "," . chunksOf 3 . stripString $ revInteger

joinAmps :: [String] -> String
joinAmps = intercalate " & " 


stripSplitCommas :: String -> [String]
stripSplitCommas s = 
  map (unpack . T.strip). T.splitOn (pack ",") $ pack s


doSubstitution :: String  -- Input String
               -> [(Text, Text)] -- Substitutions
               -> String -- Transformed string
doSubstitution src = 
  unpack . foldl (\s (a,b) -> T.replace a b s) (pack src)



stripString :: String -> String
stripString = unpack . T.strip . pack



type CommentParser = Parsec String ()

removeCommentsEIO :: String -> EIO ErrorMsg String 
removeCommentsEIO = hoistEither . removeComments

removeComments :: String -> Either ErrorMsg String
removeComments s =
  case runParser commentedText () "Comment removal source" s of 
    Left  e -> Left $ show e
    Right r -> Right r

commentedText :: CommentParser String
commentedText = unlines <$> endBy commentLine eol

commentLine :: CommentParser String
commentLine = concat <$> (many escapedChar <* optional ( char '#' <* many (noneOf "\n\r")) )

escapedChar :: CommentParser String
escapedChar = ((:) <$> (char '\\' *> anyChar) <*> return [])
           <|>((:[]) <$> noneOf "#\n\r") 


commentChar :: CommentParser Char
commentChar = char '#'

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

lowercase = map toLower

