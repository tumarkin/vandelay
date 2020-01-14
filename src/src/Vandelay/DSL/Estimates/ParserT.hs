module Vandelay.DSL.Estimates.ParserT
  ( readEstimates
  ) where

import           Control.Monad.Error.Class
import qualified Control.Monad.State       as S
import qualified Data.Map.Lazy             as HM
import           Text.Parsec               hiding (many, optional, (<|>))

import           Vandelay.DSL.Core         hiding (try)



type EstParser m = ParsecT String () m

readEstimates ∷ (MonadError ErrorMsg m, MonadIO m)
                 ⇒ String  -- File name
                 → m EstimatesHM
readEstimates f = do
  rf <- safeReadFileWithError f "Estimates file"
  runParserT (estimatesP f) () ("Estimates file: " ++ f) (unpack rf) >>= \case
    Left  parseErr → throwError $ tshow parseErr
    Right est      → return est


-- Parser
estimatesP ∷ (MonadError ErrorMsg m, MonadIO m) 
           ⇒ String -- Filename
           → EstParser m EstimatesHM
estimatesP fileName = do
  _models <- header
  let models = map pack _models
      numCols = length models
  rows   <- many (rowOfLength numCols <* eol)

  let fcn = formCoeffs models rows

  return $ singletonMap fileName fcn


-- Header
header ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m [String]
header = unsafeTail <$> sepBy (many (noneOf "\n\r\t") ) tab <* eol

-- Data row
rowOfLength ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ Int → EstParser m (CoefCmd, [DataItem])
rowOfLength i = (,) <$> (coefcmd <* tab) <*> sepByN i cell tab

cell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
cell =  try numberCell <|> emptyCell <|> textCell

-- Coefficient commands
coefcmd ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
coefcmd =  try adddata
       <|> newcoef

adddata ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
adddata = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return AddData

newcoef ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
newcoef = NewCoef <$> many (noneOf "\t\n\r")


-- DataItems
textCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
textCell = StrData . pack <$> many (noneOf "\t\n\r")

emptyCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
emptyCell = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return BlankData

numberCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
numberCell = ValData <$> double <*> sigStars

sigStars ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m Int
sigStars = length <$> many (char '*')


-- Parser tools
sepByN ∷ (MonadError ErrorMsg m, MonadIO m)
       ⇒ Int
       → EstParser m a
       → EstParser m sep
       → EstParser m [a]
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

