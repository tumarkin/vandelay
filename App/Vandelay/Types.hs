module App.Vandelay.Types
  ( 
  -- Core monad type and reexports
    EIO
  , liftIO
  , hoistEither
  , left
  , right

  ) where

-- import App.Vandelay.Template.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Either


type EIO a = EitherT a IO

