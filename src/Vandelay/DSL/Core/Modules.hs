module Vandelay.DSL.Core.Modules
  ( module X

    -- * Control
  , EIO
  , ErrorMsg
  , prependError
  , hoistEitherError

  ) where

import           ClassyPrelude             as X
import           Control.Monad.Except      as X (ExceptT (..), runExceptT,
                                                 throwError)
import           Data.Monoid               as X (Last (..))
import           Lens.Simple               as X hiding ((<>))

import           Control.Monad.Error.Class (MonadError)



-- Either IO Monad
type EIO a    = ExceptT a IO
type ErrorMsg = Text

-- | Prepend Error
prependError ∷ Text → EIO ErrorMsg a → EIO ErrorMsg a
prependError t m = ExceptT $
  runExceptT m >>= \case
    Left  e → return (Left (t<> e))
    Right a → return (Right a)

hoistEitherError ∷ (MonadError Text m) ⇒ Either ErrorMsg a → m a
hoistEitherError e =
  case e of
    Left e' → throwError e'
    Right r → return r
