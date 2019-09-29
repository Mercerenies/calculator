{-# LANGUAGE LambdaCase, Arrows #-}

module Data.Calc.Util(untilFixed, untilFixedM,
                      maybeToMonoid, mappendMap, mapAccum, accumSomeValues,
                      stripString,
                      duplicateApply, duplicateApplyM, possibly,
                      maybeToFail, the,
                      pairwiseAttempt, pairwiseAttemptM) where

import Prelude hiding (fail)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(mapAccumL)
import Data.Monoid
import Data.Functor.Identity
import qualified Data.Text as T
import Control.Arrow
import Control.Monad hiding (fail)
import Control.Monad.Fail
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

possibly :: ArrowChoice a => a b Bool -> a b b -> a b b
possibly p f = proc x -> do
                 cond <- p -< x
                 if cond
                 then f -< x
                 else returnA -< x

maybeToFail :: MonadFail m => Maybe a -> m a
maybeToFail Nothing = fail "Nothing"
maybeToFail (Just x) = pure x

the :: Eq a => [a] -> Maybe a
the [] = Nothing
the (x:xs) = x <$ guard (all (== x) xs)

pairwiseAttempt :: (a -> a -> Maybe a) -> [a] -> [a]
pairwiseAttempt f = runIdentity . pairwiseAttemptM (\x y -> Identity (f x y))

pairwiseAttemptM :: Monad m => (a -> a -> m (Maybe a)) -> [a] -> m [a]
pairwiseAttemptM f = go
    where go [] = pure []
          go [x] = pure [x]
          go (x:y:zs) = f x y >>= \case
                          Nothing -> (x :) <$> go (y:zs)
                          Just xy -> go (xy : zs)
