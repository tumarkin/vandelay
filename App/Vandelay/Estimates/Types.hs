module App.Vandelay.Estimates.Types
  ( Estimates(..)
  , Coeff
  , Cell
  , DataItem(..)

  , OutputRequest(..)
  , defaultOutputRequest

  ) where  

import App.Vandelay.Latexable
import App.Vandelay.Text 
import Data.Monoid


data Estimates  = 
  Estimates { models       :: [String]
            , coefficients :: [Coeff] -- [(String, Coeff)]
            }
            deriving Show

type Coeff        = (CoefName, [Cell])
type Cell         = [DataItem]
data DataItem     = StrData String
                  | ValData  Double Significance 
                  | BlankData
                  deriving (Show, Eq)
type Significance = Int
type CoefName = String





instance Monoid DataItem where 
  mempty  = BlankData
  mappend a BlankData = a
  mappend BlankData b = b
  mappend a         b = a

instance Latexable DataItem where
  texify _   (StrData s) = "{" ++ s ++ "}"
  texify _   (BlankData)    = "" 
  texify fmt (ValData v s)  = commaPrintf fmt v ++ makeStars s

makeStars :: Int -> String
makeStars i | i == 0    = ""
            | otherwise = "\\textsymbol{" ++ (concat . replicate i $ "*") ++ "}"



-- Output request

data OutputRequest =
  OutputRequest { oName    :: String
                , oCoeffs  :: [String]  
                , oItemIdx :: Int
                , oFormat  :: String
                , oSurround :: (String, String)
                }
                deriving (Show)

defaultOutputRequest =
  OutputRequest { oName  = ""
                , oCoeffs = []
                , oItemIdx = 0
                , oFormat  = "%1.3f"
                , oSurround = ("", "") 
                }
