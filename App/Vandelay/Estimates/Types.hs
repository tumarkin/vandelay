module App.Vandelay.Estimates.Types
  ( Estimates(..)
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
  , getOEmpty

  -- | These are not currently used
  , setOName 
  , setOCoeffs
  , setOItemIdx
  , setOFormat
  , setOSurround

  , texify

  ) where  

import App.Vandelay.Core 
import Control.Arrow
-- import Data.Char
-- import Data.List


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
texify :: OutputRequest -> DataItem -> String
texify _  (StrData s)   = "{" ++ s ++ "}"
texify or  BlankData    = getOEmpty or 
texify or (ValData v s) = changeAllZeros (surroundText or (commaPrintf (getOFormat or) (getOScale or * v))) ++ makeStars s

makeStars :: Int -> String
makeStars i | i == 0    = ""
            | otherwise = "\\sym{" ++ (concat . replicate i $ "*") ++ "}"

surroundText :: OutputRequest -> String -> String
surroundText or s = let (prefix, postfix) = getOSurround or
                     in prefix ++ s ++ postfix

changeAllZeros :: String -> String
changeAllZeros s = if   any (`elem` ['1'..'9']) s then s
                   else changeLast0 . addLessThan $ s
  where
    addLessThan = replaceFirst '0' "$<$0" 
    changeLast0 = replaceLast '0' "1" 

replaceFirst :: (Eq a) => a -> [a] -> [a] -> [a]
replaceFirst cFrom sTo = uncurry (++) . second ( \x -> sTo ++ tail x) . break (== cFrom)

replaceLast :: (Eq a) => a -> [a] -> [a] -> [a]
replaceLast cFrom sTo = reverse . replaceFirst cFrom sTo . reverse 




-- Output request

data OutputRequest =
  OutputRequest { oName     :: Last String
                , oCoeffs   :: Last [String]
                , oItemIdx  :: Last Int
                , oFormat   :: Last String
                , oSurround :: Last (String, String)
                , oScale    :: Last Double
                , oEmpty    :: Last String
                }
                deriving (Show, Ord, Eq)

getOName     = fromJust . getLast . oName
getOCoeffs   = fromJust . getLast . oCoeffs
getOItemIdx  = fromJust . getLast . oItemIdx
getOFormat   = fromJust . getLast . oFormat
getOSurround = fromJust . getLast . oSurround
getOScale    = fromJust . getLast . oScale
getOEmpty    = fromJust . getLast . oEmpty

defaultOutputRequest =
  OutputRequest { oName     = Last $ Just ""
                , oCoeffs   = Last $ Just []
                , oItemIdx  = Last $ Just 0
                , oFormat   = Last $ Just "%1.3f"
                , oSurround = Last $ Just ("", "")
                , oScale    = Last $ Just 1
                , oEmpty    = Last $ Just ""
                }

blankOutputRequest = 
  OutputRequest { oName     = Last Nothing
                , oCoeffs   = Last Nothing
                , oItemIdx  = Last Nothing
                , oFormat   = Last Nothing
                , oSurround = Last Nothing
                , oScale    = Last Nothing
                , oEmpty    = Last Nothing
                }

instance Monoid OutputRequest where
  mempty  = blankOutputRequest
  mappend a b = 
    OutputRequest { oName     = oName     a <> oName     b 
                  , oCoeffs   = oCoeffs   a <> oCoeffs   b 
                  , oItemIdx  = oItemIdx  a <> oItemIdx  b 
                  , oFormat   = oFormat   a <> oFormat   b 
                  , oSurround = oSurround a <> oSurround b 
                  , oScale    = oScale    a <> oScale    b 
                  , oEmpty    = oEmpty    a <> oEmpty    b 
                  }
  
setOName    t      = blankOutputRequest{oName = Last . Just $ t}
setOCoeffs cs      = blankOutputRequest{oCoeffs = Last . Just $ cs}
setOItemIdx i      = blankOutputRequest{oItemIdx = Last . Just $ i}
setOFormat t       = blankOutputRequest{oFormat = Last . Just $ t}
setOSurround (l,r) = blankOutputRequest{oSurround = Last . Just $ (l,r)}



