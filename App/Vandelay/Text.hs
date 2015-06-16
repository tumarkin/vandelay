module App.Vandelay.Text 
  ( hasChar
  , hasDigit
  
  , splitTab
  , unwordEnglishList

  , joinAmps

  , commaPrintf

  , stripSplitCommas 

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




hasChar :: String -> Bool
hasChar = or . map isAlpha

hasDigit :: String -> Bool
hasDigit = or . map isDigit

splitTab :: String -> [String] 
splitTab s = map unpack $ T.splitOn (pack "\t") (pack s)

unwordEnglishList :: [String] -> String
unwordEnglishList (s1:[])    = s1
unwordEnglishList (s1:s2:[]) = s1 ++ " and " ++ s2
unwordEnglishList ss         = intercalate ", " (init ss) ++ ", and " ++ last ss


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


stripSplitCommas :: Text -> [String]
stripSplitCommas s = 
  map (unpack . T.strip). T.splitOn (pack ",") $ s


