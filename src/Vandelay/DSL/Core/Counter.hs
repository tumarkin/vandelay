-----------------------------------------------------------------------------
-- |
-- Module      :  Counter
-- Updated     : 2018-Jan-02
--
-- A counter for ordered items, either individually or in a foldable.
--
-----------------------------------------------------------------------------
module Vandelay.DSL.Core.Counter
  ( Counter(..)
  , emptyCounter
  , count
  , countItem
  , countItem'
  , countItems
  , countItems'
  , listifyCounter
  ) where

import           RIO
import qualified RIO.Map    as M

newtype Counter a = Counter { unCounter :: M.Map a Int }
  deriving (Show, Eq)

instance Ord a => Semigroup (Counter a) where
  ca <> cb = Counter $ M.unionWith (+) ca.unCounter cb.unCounter 

instance Ord a => Monoid (Counter a) where
  mempty        = emptyCounter

emptyCounter :: Counter a
emptyCounter = Counter M.empty

count :: (Foldable f, Ord a) => f a -> Counter a
count = foldr countItem' emptyCounter

countItem :: (Ord a) => Counter a -> a -> Counter a
countItem c a = Counter $ M.insertWith (+) a 1 c.unCounter

countItem' :: (Ord a) => a -> Counter a -> Counter a
countItem' = flip countItem

countItems :: (Foldable f, Ord a) => Counter a -> f a -> Counter a
countItems c as = c <> count as

countItems' :: (Foldable f, Ord a) => f a -> Counter a -> Counter a
countItems' = flip countItems

listifyCounter ∷ Counter a → [(a, Int)]
listifyCounter = M.toList . (.unCounter)

