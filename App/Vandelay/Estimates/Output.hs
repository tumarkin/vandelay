module App.Vandelay.Estimates.Output
  ( outputRow
  ) where  

-- import Debug.Trace

import App.Vandelay.Latexable
import App.Vandelay.Text 
import App.Vandelay.Estimates.Types
import Data.List
import Data.Maybe
import Data.Monoid

-- import App.Vandelay.Estimates.Parser
-- import Control.Monad
-- import Control.Monad.IO.Class


outputRow :: Estimates     -- Estimates
          -> [String]      -- Models
          -> OutputRequest -- Output request
          -> Either String String 
outputRow  est ms or = do
  midx   <- getModelIndices ms est
  coefs  <- getCoefficientCells (oCoeffs or) est
  result <- mapM (combineCoeffCellItems coefs (oItemIdx or)) midx

  let (prefix, postfix) = oSurround or

  Right $ joinAmps ( oName or : -- Name     
                   (map (\s -> prefix ++ s ++ postfix) . map (texify (oFormat or)) $ result)) --------Formatted   Values   -------
  


combineCoeffCellItems :: [[Cell]]
                      -> Int
                      -> Int
                      -> Either String DataItem
combineCoeffCellItems  cells itemidx modelidx = 
      mapM (safeCellLookup itemidx) cellsForModels -- either string [dataitem]
  >>= return . mconcat
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


 



-- Debugging
-- e = Estimates 
--   { models = ["total_act_ret_uv","total_act_ret_mv","board_act_ret_uv","board_act_ret_mv"]
--   , coefficients = 
--       [ ("ann_return",[[ValData (-6.5280000000000005) 3,ValData (-8.514) 0],[ValData (-4.864) 3,ValData (-7.007) 0],[ValData (-4.338) 3,ValData (-7.665) 0],[ValData (-3.484) 3,ValData (-6.744) 0]])
--       , ("log_board_size",[[BlankData,BlankData],[ValData 92.1 3,ValData 40.598 0],[BlankData,BlankData],[ValData 60.179 3,ValData 40.696 0]])
--       ]
--    }


-- outreq = defaultOutputRequest{oName="Variable", oCoeffs=["log_board_size","ann_return"]} 
-- ms = ["board_act_ret_uv","total_act_ret_mv"]

-- debug = outputRow e ms outreq 


