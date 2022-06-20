module Vandelay.DSL.Core.Modules
  ( module X

    -- * Control
  , ErrorMsg
  , prependError
  , hoistEitherError

  ) where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      as X (ExceptT (..), runExceptT,
                                                 throwError)
import           Data.Monoid               as X (Last (..))
import           Data.NonNull              as X (fromNullable, toNullable)
import           RIO                       as X


-- Either IO Monad
type ErrorMsg = Text

-- | Prepend Error
prependError ∷ Text → ExceptT ErrorMsg (RIO env) a → ExceptT ErrorMsg (RIO env) a
prependError t m = ExceptT $
  runExceptT m >>= \case
    Left  e → return (Left (t<> e))
    Right a → return (Right a)

hoistEitherError ∷ (MonadError Text m) ⇒ Either ErrorMsg a → m a
hoistEitherError e =
  case e of
    Left e' → throwError e'
    Right r → return r
