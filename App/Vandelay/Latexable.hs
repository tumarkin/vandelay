module App.Vandelay.Latexable
  ( Latexable
  , texify
  ) where

class Latexable a where 
  texify  :: String -- Format 
          -> a      
          -> String



