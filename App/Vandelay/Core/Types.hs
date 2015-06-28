module App.Vandelay.Core.Types
  ( 
  -- Core monad type and reexports
    EIO
  , liftIO
  , hoistEither
  , left
  , right

  -- Monoid reexport
  , module Data.Monoid
  , module Data.Maybe

  -- Ordinal 
  , Ordinal(..)
  , sortExtractOrdinal


  , runEitherT

  -- --  Latexable
  -- , Latexable
  -- , texify

  ) where

-- import App.Vandelay.Template.Types
import Data.List
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Monoid
import Data.Maybe


-- Either IO Monad 
type EIO a = EitherT a IO


-- Ordinal to add higher level ordering to ord data
data Ordinal a = Ordinal{ order  :: Int
                        , getOrd :: a
                        }
                        deriving (Eq, Ord)

sortExtractOrdinal :: (Ord a) 
                   => [Ordinal a]
                   -> [a] 
sortExtractOrdinal = map getOrd . sort




-- -- Latexable

-- class Latexable a where 
--   texify  :: String -- Format 
--           -> a      
--           -> String



