module Vandelay.DSL.Core.Error (
    ErrorMsg,
    prependError,
)
where

import Control.Monad.Except
import RIO

-- Either IO Monad
type ErrorMsg = Text

-- | Prepend Error
prependError ∷ Text → ExceptT ErrorMsg (RIO env) a → ExceptT ErrorMsg (RIO env) a
prependError t m =
    ExceptT $ runExceptT m >>= \case
        Left e → return (Left (t <> e))
        Right a → return (Right a)
