module App.Vandelay.Estimates.Output
  ( outputRow
  ) where  

import Data.Either

import App.Vandelay.Core 
import App.Vandelay.Estimates.Types


-- Output a row

outputRow :: OutputRequest 
          -> [Estimates]
          -> [ModelName]    
          -> Either String String 
outputRow  or est ms = do
  result <- mapM (concatDataItems or est) ms 
  Right $ joinAmps ( getOName or : -- Name     
                     map (surroundText or . texify (getOFormat or)) result
                   ) 
          ++ "\\\\"

--- 
concatDataItems :: OutputRequest -> [Estimates] -> ModelName -> Either String DataItem 
concatDataItems  o es mn | null dataItems = Left $ unwords ["Coefficients", unwordEnglishList (getOCoeffs o), "not found in specification", mn, "\nUse coefficient 'missing' to allow for missing data if this is desired."]
                         | otherwise      = Right $ mconcat dataItems
    where 
  dataItems = rights (map (lookupDataItem o es mn) . getOCoeffs $ o) 
    


lookupDataItem :: OutputRequest -> [Estimates] -> ModelName -> CoefName -> Either String DataItem
lookupDataItem o e m c | c == "missing" = Right BlankData
                       | otherwise      =
                            (!!) <$> lookupCell m c e <*> return (getOItemIdx o) 


lookupCell :: ModelName -> CoefName -> [Estimates] -> Either String Cell
lookupCell m c e = 
  lookupCellInEst m c =<< findEstimatesWithModel m e


lookupCellInEst :: ModelName -> CoefName -> Estimates -> Either String Cell
lookupCellInEst m c e | m `notElem` models e       = Left noModelErr
                      | c `notElem` coefficients e = Left noCoeffErr
                      | otherwise                  = Right . fromJust . lookup (m,c) . eData $ e
  where noModelErr = unwords ["Model", m, "not found in estimates", sourceFile e ]
        noCoeffErr = unwords ["Coefficient", c, "not found in estimates", sourceFile e]


findEstimatesWithModel :: ModelName -> [Estimates] -> Either String Estimates
findEstimatesWithModel m es | null validEst       = Left noModelErr
                            | length validEst > 1 = Left multiModelErr
                            | otherwise           = Right $ head validEst
  where validEst      = filter (elem m . models) es
        noModelErr    = unwords ["Model", m, "not found in data files", unwordEnglishList (map sourceFile es) ]
        multiModelErr = unwords ["Model", m, "found in multiple data files.\nFound in ", unwordEnglishList (map sourceFile validEst)]

surroundText :: OutputRequest -> String -> String
surroundText or s = let (prefix, postfix) = getOSurround or
                     in prefix ++ s ++ postfix



