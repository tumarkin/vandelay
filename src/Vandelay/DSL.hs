module Vandelay.DSL where

import           Control.Monad.Error.Class
import           Control.Monad.RWS
import qualified RIO.Text                  as T
import           Vandelay.DSL.Core
import           Vandelay.DSL.Estimates


type VandelayT m = RWST Models LaTeX EstimatesHM m
type VandelayM = VandelayT (ExceptT ErrorMsg IO)

type Models = [(FilePath, ModelName)]
type LaTeX  = Text

createTable ∷ (MonadError ErrorMsg m, MonadIO m)
            ⇒ [FilePath]     -- ^ Estimates files
            → Models         -- ^ Models
            → VandelayT m () -- ^ Table creation commands
            → m Text
createTable es ms tbl = do
    ests          <- mconcat <$> mapM readEstimates es
    (_, _, latex) <- runRWST tbl ms ests
    return latex

addText ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ Text → VandelayT m ()
addText = tell

dataRow ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ OutputRequest → VandelayT m ()
dataRow or = 
  outputRow or <$> get <*> ask >>= \case
    Left e  → throwError e
    Right t → tell t

addColumns ∷ (MonadError ErrorMsg m, MonadIO m) ⇒ [Text] → VandelayT m ()
addColumns = tell . T.intercalate " & "



