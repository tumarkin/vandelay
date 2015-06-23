module App.Vandelay.Estimates.Output
  ( outputRow
  ) where  

import Data.List
import Data.Maybe
import Data.Monoid

import App.Vandelay.Text 
import App.Vandelay.Types
import App.Vandelay.Estimates.Types



outputRow :: OutputRequest -- Output request
          -> Estimates
          -> [String]     --  [Models]
          -> Either String String 
outputRow  or est ms = do
  midx   <- getModelIndices ms est
  coefs  <- getCoefficientCells (getOCoeffs or) est
  result <- mapM (combineCoeffCellItems coefs (getOItemIdx or)) midx

  let (prefix, postfix) = getOSurround or

  Right $ joinAmps ( getOName or : -- Name     
                   (map (\s -> prefix ++ s ++ postfix) . map (texify (getOFormat or)) $ result)) --------Formatted   Values   -------
          ++ "\\\\"

combineCoeffCellItems :: [[Cell]]
                      -> Int
                      -> Int
                      -> Either String DataItem
combineCoeffCellItems  cells itemidx modelidx = 
  return . mconcat =<< mapM (safeCellLookup itemidx) cellsForModels -- either string [dataitem]
    where 
  cellsForModels = map (!! modelidx) $ cells


safeCellLookup :: Int -> Cell -> Either String DataItem
safeCellLookup i c | i >= length c = Left $ "Cell item at index " ++ show i ++ " does not exist."
                   | otherwise     = Right $ c !! i


getModelIndices :: [String] -> Estimates -> Either String [Int]
getModelIndices mnames est 
  | missingmodels /= [] = Left  $ missingmodelerror 
  | otherwise           = Right $ indices 
    where
  missingmodelerror = unlines [ unwords ["Model(s):", unwordEnglishList missingmodels, "not found in estimation results file."]
                              , unwords ["Available models are:", unwordEnglishList (models est)]
                              ]
  missingmodels = catMaybes . map ( \x -> if x `elem` (models est) then Nothing else Just x ) $ mnames
  indices       = catMaybes . map ( \x -> (==) x `findIndex` (models est) ) $ mnames


getCoefficientCells :: [String] -> Estimates -> Either String [[Cell]]
getCoefficientCells cnames est | missingcoefs /= [] = Left  $ "Coefficient(s): " ++ unwordEnglishList missingcoefs ++ " not found in estimation results file." 
                               | otherwise          = Right $ cells
    where
  missingcoefs  = catMaybes . map ( \x -> case x `lookup` (coefficients est) of 
                                             Nothing -> Just x
                                             _       -> Nothing ) $ cnames
  cells         = catMaybes . map ( `lookup` (coefficients est) ) $ cnames


 


