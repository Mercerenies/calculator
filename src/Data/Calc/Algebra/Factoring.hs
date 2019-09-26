{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Algebra.Factoring where

import Data.Calc.Expr
import Data.Calc.Util

import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Semigroup

type MatchFunction a = Expr a -> Maybe (a, Expr a)

type CoalesceFunction a = a -> [Expr a] -> Expr a

data FractionBar = Denominator | Numerator
                   deriving (Show, Read, Eq, Ord, Bounded, Enum)

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

instance Semigroup FractionBar where
    Denominator <> Numerator = Denominator
    Denominator <> Denominator = Numerator
    Numerator   <> x = x

instance Monoid FractionBar where
    mempty = Numerator
    mappend = (<>)

simply :: String -> [Expr a] -> Expr a
simply _ [x] = x
simply s xs  = Compound s xs
