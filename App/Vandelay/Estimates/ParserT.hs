module App.Vandelay.Estimates.ParserT
  ( readEstimatesEIO
  ) where  

-- import Debug.Trace

import App.Vandelay.Estimates.Types
import App.Vandelay.IO
import App.Vandelay.Types
import App.Vandelay.Text

import Control.Applicative
import Control.Monad.Trans.Either
import Data.List
import Text.Parsec hiding (many, optional, (<|>))


type EstParser = ParsecT String () (EIO String)



readEstimatesEIO :: String  -- File name
                 -> EitherT String IO Estimates
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

  return $ Estimates fileName models (formCoeffs rows)



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
numberCell = ValData <$> number <*> sigStars 

number = parNegNumber <|> negativeNumber <|> unsignedNumber

parNegNumber   = (0-) <$> (char '(' *> unsignedNumber <* char ')')
negativeNumber = (0-) <$> (char '-' *> unsignedNumber)

unsignedNumber :: EstParser Double
unsignedNumber =  try (read3 <$> many1 digit <*> string "." <*> many1 digit) 
  <|> try (read3 <$> many1 digit <*> string "." <*> return "0" ) 
  <|> try (read3 <$> return "0"  <*> string "." <*> many1 digit) 
  <|> try (read3 <$> many1 digit <*> return ""  <*> return ""  ) 
    where 
  read3 a b c = read (a++b++c)

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
makeNewCoef (NewCoef n, is) = Coeff n (listToListOfLists is)

combineData :: Coeff -> [DataItem] -> Coeff
combineData c ds = c{cCells = zipWith (++) (cCells c) (listToListOfLists ds)}


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

