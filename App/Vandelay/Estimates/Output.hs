module App.Vandelay.Estimates.Output
  ( outputRow
  ) where  

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid

import App.Vandelay.Text 
import App.Vandelay.Types
import App.Vandelay.Estimates.Types

type ModelName = String
type CoeffName = String



outputRow :: OutputRequest 
          -> [Estimates]
          -> [ModelName]    
          -> Either String String 
outputRow  or est ms = do
  
  result <- mapM (concatDataItems or est) ms 
  let (prefix, postfix) = getOSurround or

  Right $ joinAmps ( getOName or : -- Name     
                     map ((\s -> prefix ++ s ++ postfix) . texify (getOFormat or)) result
                   ) 
          ++ "\\\\"



concatDataItems :: OutputRequest
                 -> [Estimates]
                 -> ModelName
                 -> Either String DataItem 
concatDataItems  o es mn = 
  mconcat <$> (mapM (lookupDataItem o es mn) . getOCoeffs $ o)


lookupDataItem :: OutputRequest 
               -> [Estimates]
               -> ModelName
               -> CoeffName
               -> Either String DataItem
lookupDataItem o e m c =
  (!!) <$> lookupCell e m c <*> return (getOItemIdx o) 



lookupCell :: [Estimates]
           -> ModelName
           -> CoeffName 
           -> Either String Cell
lookupCell e m c = do
  est  <- findEstimatesWithModel m e
  (!!) <$> findCoefficient c est <*> findColumnIndex m est


findEstimatesWithModel :: ModelName -> [Estimates] -> Either String Estimates
findEstimatesWithModel m es | null validEst       = Left $ unwords ["Model", m, "not found in data files", unwordEnglishList (map sourceFile es) ]
                            | length validEst > 1 = Left $ unwords ["Multiple specifications of modelname found in data files.\nFound in ", unwordEnglishList (map sourceFile validEst)]
                            | otherwise           = Right $ head validEst
  where validEst = filter (elem m . models) es


findColumnIndex :: ModelName -> Estimates -> Either String Int
findColumnIndex m e = 
  case col of
    Nothing -> Left $ unwords ["Model", m, "not found in", sourceFile e ]
    Just i  -> Right i
    where 
  col = (== m) `findIndex` models e


findCoefficient :: CoeffName -> Estimates -> Either String [Cell]
findCoefficient cn = findCoefficient' cn . coefficients


findCoefficient' :: CoeffName -> [Coeff] -> Either String [Cell]
findCoefficient' s []     = Left $ "Coefficient " ++ s ++ " not found."
findCoefficient' s (c:cs) | cName c ==s = Right $ cCells c
                          | otherwise   = findCoefficient' s cs




