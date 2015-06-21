module App.Vandelay.Estimates.Types
  ( Estimates(..)
  , Coeff
  , Cell
  , DataItem(..)

  , OutputRequest(..)
  , defaultOutputRequest
  , blankOutputRequest

  , getOName
  , getOCoeffs
  , getOItemIdx
  , getOFormat
  , getOSurround

  , setName
  , setCoeffs
  , setItemIdx
  , setFormat
  , setSurround

  ) where  

import App.Vandelay.Text 
import App.Vandelay.Types


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
  OutputRequest { oName     :: Last String
                , oCoeffs   :: Last [String]
                , oItemIdx  :: Last Int
                , oFormat   :: Last String
                , oSurround :: Last (String, String)
                }
                deriving (Show, Ord, Eq)

getOName     = fromJust . getLast . oName
getOCoeffs   = fromJust . getLast . oCoeffs
getOItemIdx  = fromJust . getLast . oItemIdx
getOFormat   = fromJust . getLast . oFormat
getOSurround = fromJust . getLast . oSurround

defaultOutputRequest =
  OutputRequest { oName     = Last $ Just ""
                , oCoeffs   = Last $ Just []
                , oItemIdx  = Last $ Just 0
                , oFormat   = Last $ Just "%1.3f"
                , oSurround = Last $ Just ("", "")
                }

blankOutputRequest = 
  OutputRequest { oName     = Last Nothing
                , oCoeffs   = Last Nothing
                , oItemIdx  = Last Nothing
                , oFormat   = Last Nothing
                , oSurround = Last Nothing
                }

instance Monoid OutputRequest where
  mempty  = blankOutputRequest
  mappend a b = 
    OutputRequest { oName     = oName     a <> oName     b 
                  , oCoeffs   = oCoeffs   a <> oCoeffs   b 
                  , oItemIdx  = oItemIdx  a <> oItemIdx  b 
                  , oFormat   = oFormat   a <> oFormat   b 
                  , oSurround = oSurround a <> oSurround b 
                  }
  
setName    t      = blankOutputRequest{oName = Last . Just $ t}
setCoeffs cs      = blankOutputRequest{oCoeffs = Last . Just $ cs}
setItemIdx i      = blankOutputRequest{oItemIdx = Last . Just $ i}
setFormat t       = blankOutputRequest{oFormat = Last . Just $ t}
setSurround (l,r) = blankOutputRequest{oSurround = Last . Just $ (l,r)}



