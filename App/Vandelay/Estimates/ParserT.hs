module App.Vandelay.Estimates.ParserT
  ( readEstimatesEIO
  ) where  

-- import Debug.Trace

import Text.Parsec hiding (many, optional, (<|>))

import App.Vandelay.Estimates.Types
import App.Vandelay.Core
import App.Vandelay.Shared.ParserT




type EstParser = ParsecT String () (EIO String)



readEstimatesEIO :: String  -- File name
                 -> EIO String Estimates
readEstimatesEIO f = do
  txt <- safeReadFileWithError f "Estimates file" 
  r   <- runParserT (estimates f) () ("Estimates file: " ++ f) txt 
  case r of 
    Left  parseErr -> left $ show parseErr
    Right est       -> right est 


-- Parser 
estimates :: String -- Filename
          -> EstParser Estimates
estimates fileName = do 
  models <- header 
  let numCols = length models 
  rows   <- many (rowOfLength numCols <* eol)
  let eData = formCoeffs models rows

  return Estimates{ sourceFile   = fileName
                  , models       = models
                  , coefficients = nub . map (snd . fst) $ eData
                  , eData        = eData
                  } 



-- Header 
header :: EstParser [String]  
header = tail <$> (sepBy (many (noneOf "\n\r\t") ) tab <* eol)

-- Data row
rowOfLength :: Int -> EstParser (CoefCmd, [DataItem])
rowOfLength i = (,) <$> (coefcmd <* tab) <*> sepByN i cell tab

cell :: EstParser DataItem
cell =  try numberCell <|> emptyCell <|> textCell

-- Coefficient commands
coefcmd =  try adddata 
       <|> newcoef

adddata = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return AddData
newcoef = NewCoef <$> many (noneOf "\t\n\r")


-- DataItems
textCell :: EstParser DataItem
textCell = StrData <$> many (noneOf "\t\n\r") 

emptyCell :: EstParser DataItem
emptyCell = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return BlankData

numberCell :: EstParser DataItem
numberCell = ValData <$> double <*> sigStars 

sigStars :: EstParser Int
sigStars = length <$> many (char '*')


-- Parser tools
eol :: EstParser String
eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"


sepByN :: Int
       -> EstParser a
       -> EstParser sep
       -> EstParser [a]
sepByN 1 p sep = (:) <$>  p         <*> pure []
sepByN n p sep = (:) <$> (p <* sep) <*> sepByN (n-1) p sep




-- Make coefficients from the individual rows
formCoeffs :: [ModelName] -> [ (CoefCmd,[DataItem]) ] -> [EData]
formCoeffs ms cs = formCoeffs' ms cs (Nothing ,[]) 

formCoeffs' :: [ModelName] 
            -> [(CoefCmd,[DataItem])]   -- Command Queue
            -> ( Maybe [EData], [EData] ) -- Active, [Processed]
            -> [EData]
formCoeffs' ms []                     (Just cur, eds)  = reverse (cur ++ eds)
formCoeffs' ms (cc@(NewCoef _,_):ccs) (Nothing,[])     = formCoeffs' ms ccs (Just (makeNewCoef ms cc), [])
formCoeffs' ms (cc@(AddData  ,_):ccs) (Nothing,[])     = formCoeffs' ms ccs (Nothing,[])
formCoeffs' ms (cc@(NewCoef _,_):ccs) (Just cur,coefs) = formCoeffs' ms ccs (Just (makeNewCoef ms cc), cur ++ coefs)
formCoeffs' ms (    (AddData, d):ccs) (Just cur,coefs) = formCoeffs' ms ccs (Just (combineData cur d), coefs) 


makeNewCoef :: [ModelName] -> (CoefCmd, [DataItem]) -> [EData]
makeNewCoef ms (NewCoef n, is) = zip modelXCoeff (listToListOfLists is)
  where modelXCoeff = zip ms (repeat n)

combineData :: [EData] -> [DataItem] -> [EData]
combineData = zipWith (\e d -> (fst e, snd e ++ [d]))


listToListOfLists = map (:[]) 


-- Data integrity
validateEstimates :: Estimates -> Either String Estimates
validateEstimates est | null dupmodels = Right est
                      | otherwise      = Left $ "Duplicate estimation results: " ++ error
    where 
  dupmodels = map head . filter (\x -> length x > 1) . group . sort . models $ est
  error     = unwordEnglishList dupmodels




-- Estimate formation command
data CoefCmd = NewCoef String
             | AddData
             deriving Show

