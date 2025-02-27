module Vandelay.DSL.Estimates.ParserT (
    readEstimates,
) where

import Control.Monad.Error.Class
import qualified Control.Monad.State as S
import RIO hiding (try)
import qualified RIO.List.Partial as L'
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.Parsec hiding (many, optional, (<|>))

import Vandelay.DSL.Core hiding (try)

type EstParser m = ParsecT String () m

readEstimates
    ∷ (MonadError ErrorMsg m, MonadIO m)
    ⇒ FilePath
    → m EstimatesHM
readEstimates f = do
    rf ← safeReadFileWithError f "Estimates file"
    runParserT (estimatesP f) () ("Estimates file: " ++ f) (T.unpack rf) >>= \case
        Left parseErr → throwError $ tshow parseErr
        Right est → return est

-- Parser
estimatesP
    ∷ (MonadError ErrorMsg m, MonadIO m)
    ⇒ FilePath
    → EstParser m EstimatesHM
estimatesP fileName = do
    _models ← header
    let models = map T.pack _models
        numCols = length models
    rows ← many (rowOfLength numCols <* eol)

    let fcn = formCoeffs models rows

    return $ Map.singleton fileName fcn

-- Header
header ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m [String]
header = L'.tail <$> sepBy (many (noneOf "\n\r\t")) tab <* eol

-- Data row
rowOfLength ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ Int → EstParser m (CoefCmd, [DataItem])
rowOfLength i = (,) <$> (coefcmd <* tab) <*> sepByN i cell tab

cell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
cell = try numberCell <|> emptyCell <|> textCell

-- Coefficient commands
coefcmd ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
coefcmd =
    try adddata
        <|> newcoef

adddata ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
adddata = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return AddData

newcoef ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m CoefCmd
newcoef = NewCoef <$> many (noneOf "\t\n\r")

-- DataItems
textCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
textCell = StrData . T.pack <$> many (noneOf "\t\n\r")

emptyCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
emptyCell = manyTill space (lookAhead (tab <|> (eol >> return ' '))) >> return BlankData

numberCell ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m DataItem
numberCell = ValData <$> doubleP <*> sigStars

sigStars ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ EstParser m Int
sigStars = length <$> many (char '*')

-- Parser tools
sepByN
    ∷ (MonadError ErrorMsg m, MonadIO m)
    ⇒ Int
    → EstParser m a
    → EstParser m sep
    → EstParser m [a]
sepByN 1 p sep = (:) <$> p <*> pure []
sepByN n p sep = (:) <$> (p <* sep) <*> sepByN (n - 1) p sep

-- Make coefficients from the individual rows
formCoeffs ∷ [ModelName] → [(CoefCmd, [DataItem])] → ModelHM
formCoeffs mns cmds =
    Map.unions $ zipWith Map.singleton mns coefMaps
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
    ∷ [CoefHM]
    -- ^ Coefficient Cell map
    → (CoefCmd, [DataItem])
    -- ^ Construction command
    → S.State (Maybe Text) [CoefHM]
    -- ^ (State: Last Coefficient) Updated data
processCmd chms (NewCoef cname, ds) = do
    S.put (Just $ T.pack cname)
    return $ zipWith (\chm d → Map.insert (T.pack cname) [d] chm) chms ds
processCmd chms (AddData, ds) = do
    S.get >>= \case
        Nothing → return chms -- error "processCmd with no last coefficient"
        Just lcn → return $ zipWith (\chm d → Map.insertWith (flip (<>)) lcn [d] chm) chms ds -- Flip (<>) required to preserve order

-- Estimate formation command
data CoefCmd
    = NewCoef String
    | AddData
    deriving (Show)

----------------------------------------------------------------------------------------------------
-- Utility function                                                                               --
----------------------------------------------------------------------------------------------------
foldlM ∷ (Foldable t, Monad m) ⇒ (b → a → m b) → b → t a → m b
foldlM f z0 xs = foldr c return xs z0
  where
    -- See Note [List fusion and continuations in 'c']
    c x k z = f z x >>= k
