{-# LANGUAGE LambdaCase #-}

module Data.Calc.Util(untilFixed, untilFixedM,
                      maybeToMonoid, mappendMap, mapAccum, accumSomeValues,
                      stripString,
                      duplicateApply, duplicateApplyM) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(mapAccumL)
import Data.Monoid
import Data.Functor.Identity
import qualified Data.Text as T
import Control.Arrow
import Control.Monad
import Control.Applicative

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f = go
    where go a =
              case f a of
                a' | a == a' -> a
                   | otherwise -> go a'

untilFixedM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
untilFixedM f = go
    where go a = f a >>= \case
                   a' | a == a' -> pure a
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

stripString :: String -> String
stripString = T.unpack . T.strip . T.pack

-- duplicateApply f [a, b, c] = [[f a, b, c], [a, f b, c], [a, b, f c]]
duplicateApply :: (a -> a) -> [a] -> [[a]]
duplicateApply f = runIdentity . duplicateApplyM (Identity . f)

duplicateApplyM :: Applicative m => (a -> m a) -> [a] -> m [[a]]
duplicateApplyM f = go
    where go [] = pure []
          go (x:xs) = liftA2 (:) (liftA2 (:) (f x) (pure xs)) (fmap (x :) <$> (go xs))
