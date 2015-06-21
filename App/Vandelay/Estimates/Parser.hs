module App.Vandelay.Estimates.Parser
  ( readEstimatesEIO
  ) where  

-- import Debug.Trace

import App.Vandelay.Estimates.Types
import App.Vandelay.IO
import App.Vandelay.Types
import App.Vandelay.Text
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.List
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Number(floating3)


import Text.Parsec.Prim hiding (many, optional, (<|>), try)

-- test :: IO()
-- test = do
--   result <- runEitherT $ readEstimatesEIO "test/activity_sample.txt"
--   putStrLn $ show result


readEstimatesEIO :: String  -- File name
                 -> EitherT String IO Estimates
readEstimatesEIO f = do
  txt  <- safeReadFileWithError f "Estimates file"
  case parse estimates "Estimates parsing error: " txt of 
    Left  err -> left $ show err
    Right est -> hoistEither $ validateEstimates est


-- Parser 
estimates :: GenParser Char st Estimates
estimates = do 
  models <- header 

  let numCols = length models 
  rows   <- many (rowOfLength numCols <* eol)

  return $ Estimates models (formCoeffs rows)


header :: GenParser Char st [String]  
header = tail <$> (sepBy (many (noneOf "\n\r\t") ) tab <* eol)

rowOfLength :: Int -> GenParser Char st (CoefCmd, [DataItem])
rowOfLength i = (,) <$> (coefcmd <* tab) <*> (sepByN i cell tab)

cell :: GenParser Char st DataItem
cell =  try numberCell <|> emptyCell <|> textCell


coefcmd =  try adddata 
       <|> newcoef

adddata = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return AddData
newcoef = NewCoef <$> many (noneOf "\t\n\r")


textCell   :: GenParser Char st DataItem
textCell   = StrData <$> many (noneOf "\t\n\r") 
emptyCell  = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return BlankData
numberCell = ValData <$> number <*> sigStars 

number = parNegNumber <|> negativeNumber <|> unsignedNumber

parNegNumber   = (0-) <$> (char '(' *> unsignedNumber <* char ')')
negativeNumber = (0-) <$> (char '-' *> unsignedNumber)
unsignedNumber = floating3 False

sigStars       = length <$> many (char '*')






-- Parser tools
eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"


sepByN :: Int
       -> GenParser Char st a
       -> GenParser Char st sep
       -> GenParser Char st [a]
sepByN 1 p sep = (:) <$>  p         <*> pure []
sepByN n p sep = (:) <$> (p <* sep) <*> (sepByN (n-1) p sep)





-- Make coefficients from the individual rows

formCoeffs :: [ (CoefCmd,[DataItem]) ] -> [Coeff]
formCoeffs cs = formCoeffs' cs (Nothing ,[]) 

formCoeffs' :: [(CoefCmd,[DataItem])] -- Command Queue
            -> ( Maybe Coeff, [Coeff] ) -- Active, [Processed]
            -> [Coeff]
formCoeffs' []                     (Just cur, coefs) = reverse (cur:coefs)
formCoeffs' (cc@(NewCoef _,_):ccs) (Nothing,[])      = formCoeffs' ccs (Just (makeNewCoef cc), [])
formCoeffs' (cc@(AddData  ,_):ccs) (Nothing,[])      = formCoeffs' ccs (Nothing,[])
formCoeffs' (cc@(NewCoef _,_):ccs) (Just cur,coefs)  = formCoeffs' ccs (Just (makeNewCoef cc), cur:coefs)
formCoeffs' (    (AddData, d):ccs) (Just cur,coefs)  = formCoeffs' ccs (Just (combineData cur d), coefs) 


makeNewCoef :: (CoefCmd, [DataItem]) -> Coeff
makeNewCoef (NewCoef n, is) = (n, listToListOfLists is)



combineData :: Coeff -> [DataItem] -> Coeff
combineData (name,cells) ds = (name, zipWith (++) cells (listToListOfLists ds))


listToListOfLists = map (\x -> [x])


-- Data integrity
validateEstimates :: Estimates -> Either String Estimates
validateEstimates est | dupmodels == [] = Right est
                      | otherwise      = Left $ "Duplicate estimation results: " ++ error
    where 
  dupmodels = map head . filter (\x -> length x > 1) . group . sort . models $ est
  error     = unwordEnglishList dupmodels




-- Estimate formation command
data CoefCmd = NewCoef String
             | AddData
         deriving Show

