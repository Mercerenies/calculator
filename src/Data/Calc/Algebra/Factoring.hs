{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Algebra.Factoring where

import Data.Calc.Expr
import Data.Calc.Util

import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

type MatchFunction a = Expr a -> Maybe (a, Expr a)

type CoalesceFunction a = a -> [Expr a] -> Expr a

matchStateful :: (Ord a, MonadState (Map a [Expr a]) m) => MatchFunction a -> Expr a -> m [Expr a]
matchStateful match x
    | Just (b, e) <- match x = [] <$ modify (mappendMap b [e])
    | otherwise = pure [x]

accumulateTerms :: Ord a => MatchFunction a -> [Expr a] -> ([Expr a], Map a [Expr a])
accumulateTerms match xs = runState (concat <$> mapM (matchStateful match) xs) Map.empty

collectTerms :: Ord a => MatchFunction a -> CoalesceFunction a -> [Expr a] -> [Expr a]
collectTerms match coalesce xs = leftover ++ map coalesce' (Map.toList matched)
    where (leftover, matched) = accumulateTerms match xs
          coalesce' = uncurry coalesce
