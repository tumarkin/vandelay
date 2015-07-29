module App.Vandelay.Core.Types
  ( EIO
  , ErrorMsg
  , Ordinal(..)
  , sortExtractOrdinal
  ) where

import App.Vandelay.Core.Modules



-- Either IO Monad 
type EIO a = EitherT a IO

type ErrorMsg = String


-- Ordinal to add higher level ordering to ord data
data Ordinal a = Ordinal{ order  :: Int
                        , getOrd :: a
                        }
                        deriving (Eq, Ord)

sortExtractOrdinal :: (Ord a) 
                   => [Ordinal a]
                   -> [a] 
sortExtractOrdinal = map getOrd . sort



