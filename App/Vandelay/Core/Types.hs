module App.Vandelay.Core.Types
  ( EIO
  , ErrorMsg
  , prependError
  , Ordinal(..)
  , sortExtractOrdinal
  ) where

import App.Vandelay.Core.Modules



-- Either IO Monad 
type EIO a = EitherT a IO

type ErrorMsg = String

-- | Prepend Error
prependError :: String -> EIO ErrorMsg a -> EIO ErrorMsg a
prependError s = bimapEitherT ( (++) s) id 



-- Ordinal to add higher level ordering to ord data
data Ordinal a = Ordinal{ order  :: Int
                        , getOrd :: a
                        }
                        deriving (Eq, Ord)

sortExtractOrdinal :: (Ord a) 
                   => [Ordinal a]
                   -> [a] 
sortExtractOrdinal = map getOrd . sort



