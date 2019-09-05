
module Data.Calc.Util(untilFixed, maybeToMonoid, mappendMap, mapAccum, accumSomeValues) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(mapAccumL)
import Data.Monoid
import Control.Arrow
import Control.Monad

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f = go
    where go a =
              case f a of
                a' | a == a' -> a
                   | otherwise -> go a'

maybeToMonoid :: Monoid a => Maybe a -> a
maybeToMonoid = maybe mempty id

mappendMap :: (Ord k, Monoid a) => k -> a -> Map k a -> Map k a
mappendMap k a = Map.alter (Just . (<> a) . maybeToMonoid) k

mapAccum :: (Traversable t, Monoid a) => (b -> (a, c)) -> t b -> (a, t c)
mapAccum f = mapAccumL (\a b -> first (a <>) $ f b) mempty

accumSomeValues :: (Traversable t, MonadPlus t, Monoid b) => (a -> Maybe b) -> t a -> (b, t a)
accumSomeValues f = second join . mapAccum go
    where go a = case f a of
                   Nothing -> (mempty, pure a)
                   Just x -> (x, mzero)
