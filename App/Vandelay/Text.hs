module App.Vandelay.Text 
  ( hasChar
  , hasDigit
  
  , splitTab
  , unwordEnglishList

  , joinAmps

  , commaPrintf

  , stripSplitCommas 
  , doSubstitution

  -- , unlinesTex

  -- Reexports from Text
  , Text
  , pack
  , unpack

  ) where

import Data.Char
import Data.List
import Data.List.Split
import Text.Printf
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)




hasChar  :: String -> Bool
hasDigit :: String -> Bool

hasChar  = any isAlpha
hasDigit = any isDigit




splitTab :: String -> [String] 
splitTab s = map unpack $ T.splitOn (pack "\t") (pack s)

unwordEnglishList :: [String] -> String
unwordEnglishList [s1]    = s1
unwordEnglishList [s1,s2] = s1 ++ " and " ++ s2
unwordEnglishList ss      = intercalate ", " (init ss) ++ ", and " ++ last ss


commaPrintf :: String -- Format
            -> Double -- Value
            -> String
commaPrintf fmt d = commify . printf fmt $ d

commify :: String -> String
commify s = commiint ++ fractional
    where
  (dirtyInt, fractional)  = break ( == '.' ) s
  (revPrefix, revInteger) = break isDigit . reverse $ dirtyInt
  commiint                = reverse . intercalate "," . chunksOf 3 $ revInteger

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

