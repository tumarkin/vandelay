module Vandelay.Estimates.ParserT
  ( readEstimatesEIO
  ) where

import qualified Control.Monad.State     as S
import qualified Data.Map.Lazy           as HM
import           Text.Parsec             hiding (many, optional, (<|>))

import           Vandelay.Core           hiding (try)
import           Vandelay.Shared.Counter
import           Vandelay.Shared.ParserT



type EstParser = ParsecT String () (EIO ErrorMsg)

readEstimatesEIO ∷ String  -- File name
                 → EIO ErrorMsg EstimatesHM
readEstimatesEIO f = do
  rf <- safeReadFileWithError f "Estimates file"
  runParserT (estimatesP f) () ("Estimates file: " ++ f) (unpack rf) >>= \case
    Left  parseErr → throwError $ tshow parseErr
    Right est      → return est


-- Parser
estimatesP ∷ String -- Filename
           → EstParser EstimatesHM
estimatesP fileName = do
  _models <- header
  let models = map pack _models
      numCols = length models
  rows   <- many (rowOfLength numCols <* eol)

  let fcn = formCoeffs models rows

  return $ singletonMap fileName fcn


-- Header
header ∷ EstParser [String]
header = unsafeTail <$> sepBy (many (noneOf "\n\r\t") ) tab <* eol

-- Data row
rowOfLength ∷ Int → EstParser (CoefCmd, [DataItem])
rowOfLength i = (,) <$> (coefcmd <* tab) <*> sepByN i cell tab

cell ∷ EstParser DataItem
cell =  try numberCell <|> emptyCell <|> textCell

-- Coefficient commands
coefcmd =  try adddata
       <|> newcoef

adddata = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return AddData
newcoef = NewCoef <$> many (noneOf "\t\n\r")


-- DataItems
textCell ∷ EstParser DataItem
textCell = StrData . pack <$> many (noneOf "\t\n\r")

emptyCell ∷ EstParser DataItem
emptyCell = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return BlankData

numberCell ∷ EstParser DataItem
numberCell = ValData <$> double <*> sigStars

sigStars ∷ EstParser Int
sigStars = length <$> many (char '*')


-- Parser tools
sepByN ∷ Int
       → EstParser a
       → EstParser sep
       → EstParser [a]
sepByN 1 p sep = (:) <$>  p         <*> pure []
sepByN n p sep = (:) <$> (p <* sep) <*> sepByN (n-1) p sep




-- Make coefficients from the individual rows
formCoeffs ∷ [ModelName] → [(CoefCmd,[DataItem])] → ModelHM
formCoeffs mns cmds =
    unions $ zipWith (\mn chm → singletonMap mn chm) mns coefMaps

  where
    coefMaps ∷ [CoefHM]
    coefMaps = S.evalState coefMaps' Nothing

    coefMaps' ∷ S.State (Maybe Text) [CoefHM]
    coefMaps' = foldlM processCmd baseCoefMaps cmds

    baseCoefMaps ∷ [CoefHM]
    baseCoefMaps = mns $> emptyCoefmap

    emptyCoefmap ∷ CoefHM
    emptyCoefmap = mempty


processCmd
  ∷ [CoefHM]               -- ^ Coefficient Cell map
  → (CoefCmd, [DataItem])  -- ^ Construction command
  → S.State (Maybe Text) [CoefHM] -- ^ (State: Last Coefficient) Updated data

processCmd chms (NewCoef cname, ds) = do
    S.put (Just $ pack cname)
    return $ zipWith (\chm d → insertMap (pack cname) [d] chm) chms ds

processCmd chms (AddData, ds) = do
    S.get >>= \case
      Nothing →  return chms -- error "processCmd with no last coefficient"
      Just lcn → return $ zipWith (\chm d → insertWith (flip (<>)) lcn [d] chm) chms ds -- Flip (<>) required to preserve order

-- Estimate formation command
data CoefCmd = NewCoef String
             | AddData
             deriving Show

