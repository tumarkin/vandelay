module App.Vandelay.Estimates.Types
  ( Estimates(..)
  -- , Coeff(..)
  , Cell
  , DataItem(..)

  , ModelName
  , CoefName
  , EData

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

  , texify

  ) where  

import App.Vandelay.Core 

-- import Control.Monad.Trans.Reader
import Control.Monad.RWS
import Data.List
import Control.Applicative


data Estimates  = 
  Estimates { sourceFile   :: String
            , models       :: [ModelName]
            , coefficients :: [CoefName]
            , eData        :: [ ((ModelName, CoefName), Cell) ] 
            }
            deriving Show

type EData = ((ModelName, CoefName), Cell)

-- data Coeff        = Coeff { cName  :: String
--                           , cCells :: [Cell]
--                           } deriving (Show, Eq)

type Cell         = [DataItem]
data DataItem     = StrData String
                  | ValData  Double Significance 
                  | BlankData
                  deriving (Show, Eq)
type Significance = Int

type ModelName    = String
type CoefName     = String




-- Data Item 

instance Monoid DataItem where 
  mempty  = BlankData
  mappend a BlankData = a
  mappend BlankData b = b
  mappend a         b = a

-- instance Latexable DataItem where
texify :: String -> DataItem -> String
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



