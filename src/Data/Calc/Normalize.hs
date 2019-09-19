{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, Arrows #-}

module Data.Calc.Normalize where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Repr
import Data.Calc.Util
import Data.Calc.Function
import Data.Calc.Mode
import Data.Calc.Number
import Data.Calc.Algebra.Factoring

import Prelude hiding ((.), id)
import Data.List(sort, partition)
import Data.Maybe
import Data.Monoid
import Data.Function((&))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import Control.Monad.Reader
import Control.Arrow

normalizeNegatives :: Monad m => ReprInteger a => PassT m a a
normalizeNegatives = pass subtractionPass . pass negationPass
    where subtractionPass (Compound "-" [a, b]) =
              Compound "+" [a, Compound "*" [reprInteger (-1), b]]
          subtractionPass x = x
          negationPass (Compound "_" [b]) =
              Compound "*" [reprInteger (-1), b]
          negationPass x = x

levelOperators :: Monad m => [String] -> PassT m a a
levelOperators ss = foldr (.) id $ map (pass . go) ss
    where go str (Compound str' xs)
             | str == str' = Compound str' $ concatMap (flatten str) xs
          go _ x = x
          flatten str (Compound str' xs)
              | str == str' = xs
          flatten _ x = [x]

levelStdOperators :: Monad m => PassT m a a
levelStdOperators = levelOperators ["+", "*"]

simplifyRationals :: Monad m => PassT m a a
simplifyRationals = foldr (.) id $ map pass [rule1, rule2, rule3]
    where -- (a/b)/c ==> a/(bc)
          rule1 (Compound "/" [Compound "/" [a, b], c]) = Compound "/" [a, Compound "*" [b, c]]
          rule1 x = x
          -- a/(b/c) ==> (ac)/b
          rule2 (Compound "/" [a, Compound "/" [b, c]]) = Compound "/" [Compound "*" [a, c], b]
          rule2 x = x
          -- a(b/c)d ==> (abd)/c
          rule3 (Compound "*" xs)
              | any isDivision xs =
                  let numerator = map extractNum xs
                      denominator = mapMaybe extractDenom xs
                  in Compound "/" [Compound "*" numerator, Compound "*" denominator]
          rule3 x = x
          isDivision (Compound "/" _) = True
          isDivision _ = False
          extractNum (Compound "/" [a, _]) = a
          extractNum x = x
          extractDenom (Compound "/" [_, b]) = Just b
          extractDenom _ = Nothing

collectLikeFactors :: forall a m. (ReprInteger a, HasVars a, HasNumbers a, Ord a, Monad m) =>
                      PassT m a a
collectLikeFactors = pass collect
    where collect (Compound "*" xs) = simply "*" (collectTerms match coalesce xs)
          collect x = x
          match (Constant a) | isVar a = Just (a, Constant (reprInteger 1))
          match (Compound "^" [Constant a, Constant b]) | isVar a = Just (a, Constant b)
          match _ = Nothing
          coalesce a [x] | x == Constant (reprInteger 1) = Constant a
          coalesce a es = Compound "^" [Constant a, simply "+" es]

collectLikeTerms :: forall a m. (ReprInteger a, HasVars a, HasNumbers a, Ord a, Monad m) =>
                    PassT m a a
collectLikeTerms = pass collect
    where collect (Compound "+" xs) = simply "+" (collectTerms match coalesce xs)
          collect x = x
          match (Constant a) | isVar a = Just (a, Constant (reprInteger 1))
          match (Compound "*" [Constant a, Constant b])
              | isVar a && isNumber b = Just (a, Constant b)
              | isVar b && isNumber a = Just (b, Constant a)
          match _ = Nothing
          coalesce a [x] | x == Constant (reprInteger 1) = Constant a
          coalesce a es = Compound "*" [simply "+" es, Constant a]

collectFactorsFromDenom :: forall a m. (ReprInteger a, HasVars a, HasNumbers a, Ord a, Monad m) =>
                           PassT m a a
collectFactorsFromDenom = pass collect
    where collect (Compound "/" [a, b]) =
              let as = fromProduct a
                  bs = fromProduct b
                  (nums, mnum) = accumulateTerms match as
                  (dens, mden) = accumulateTerms match bs
                  result = Merge.merge numOnly denOnly numDen mnum mden
                  (newnum, newden) = Map.toList result &
                                     partition (\(_, (_, fb)) -> fb == Numerator) &
                                     (map (\(k, (v, _)) -> (k, v)) *** map (\(k, (v, _)) -> (k, v)))
                  finalnum = nums ++ map (uncurry coalesce) newnum
                  finalden = dens ++ map (uncurry coalesce) newden
              in Compound "/" [simply "*" finalnum, simply "*" finalden]
          collect x = x
          fromProduct (Compound "*" xs) = xs
          fromProduct x = [x]
          match (Constant a) | isVar a = Just (a, Constant (reprInteger 1))
          match (Compound "^" [Constant a, Constant b]) | isVar a = Just (a, Constant b)
          match _ = Nothing
          coalesce a [x] | x == Constant (reprInteger 1) = Constant a
          coalesce a [x] = Compound "^" [Constant a, x]
          coalesce a es = Compound "^" [Constant a, Compound "+" es]
          classifyExponent [Constant x] | isNegative x = ([Compound "_" [Constant x]], Denominator)
          classifyExponent [y] = ([y], Numerator)
          classifyExponent ys = (ys, Numerator)
          numOnly = Merge.mapMissing $ \_ xs -> second (Numerator   <>) $ classifyExponent xs
          denOnly = Merge.mapMissing $ \_ ys -> second (Denominator <>) $ classifyExponent ys
          numDen  = Merge.zipWithMatched $ \_ xs ys -> ([Compound "-" [Compound "+" xs,
                                                                       Compound "+" ys]],
                                                        Numerator)

flattenNestedExponents :: Monad m => PassT m a a
flattenNestedExponents = pass go
    where go (Compound "^" [Compound "^" [a, b], c]) =
              Compound "^" [a, Compound "*" [b, c]]
          go x = x

-- TODO I'd like to generalize this to take Pass a a for appropriately typeclassed a.
foldConstants :: Monad m => PassT m Prim Prim
foldConstants = pass eval
    where eval (Compound "+" xs) =
              case accumSomeValues (fmap Sum . coerceToNum) xs of
                (Sum 0, [x]) -> x
                (Sum 0, xs') -> Compound "+" xs'
                (Sum n, xs') -> Compound "+" (Constant (PrimNum n) : xs')
          eval (Compound "*" xs) =
              case accumSomeValues (fmap Product . coerceToNum) xs of
                (Product 0,   _) -> Constant (PrimNum 0)
                (Product 1, [x]) -> x
                (Product 1, xs') -> Compound "*" xs'
                (Product n, xs') -> Compound "*" (Constant (PrimNum n) : xs')
          eval (Compound "/" [a, b])
              | Just 1 <- coerceToNum b = a
              | Just 0 <- coerceToNum a = Constant (PrimNum 0)
              -- TODO Float
              | (Just a', Just b') <- (coerceToNum a, coerceToNum b) =
                  Constant $ PrimNum (a' / b')
          eval (Compound "^" [a, b])
              | Just 1 <- coerceToNum a = Constant (PrimNum 1)
              | Just 0 <- coerceToNum b = Constant (PrimNum 1)
              | Just 1 <- coerceToNum b = a
              -- TODO Float
              | (Just a', Just b') <- (coerceToNum a, coerceToNum b) =
                  Constant $ PrimNum (a' ** b')
          eval x = x
          coerceToNum (Constant (PrimNum n)) = Just n
          coerceToNum _ = Nothing

-- TODO Generalize this to be typeclassed like above.
evalFunctions :: MonadReader ModeInfo m => PassT m Prim Prim
evalFunctions = PassT eval
    where eval (Compound h xs) = applyToStd h xs
          eval x = pure x

flattenSingletons :: Monad m => [String] -> PassT m a a
flattenSingletons ss = foldr (.) id $ map (pass . go) ss
    where go str (Compound str' [a]) | str == str' = a
          go _ x = x

flattenStdSingletons :: Monad m => PassT m a a
flattenStdSingletons = flattenSingletons ["+", "*"]

flattenNullaryOps :: Monad m => [(String, Expr a)] -> PassT m a a
flattenNullaryOps ss = foldr (.) id $ map (pass . go) ss
    where go (s, e) (Compound s' []) | s == s' = e
          go _ x = x

flattenStdNullaryOps :: (Monad m, ReprInteger a) => PassT m a a
flattenStdNullaryOps = flattenNullaryOps [("+", reprInteger 0), ("*", reprInteger 1)]

sortTermsOf :: (Ord a, Monad m) => [String] -> PassT m a a
sortTermsOf ss = foldr (.) id $ map (pass . go) ss
    where go str (Compound str' xs) | str == str' = Compound str' (sort xs)
          go _ x = x

sortTermsOfStd :: (Ord a, Monad m) => PassT m a a
sortTermsOfStd = sortTermsOf ["+", "*"]

promoteRatios :: Monad m => PassT m Prim Prim
promoteRatios = pass go
    where go (Constant (PrimNum (NRatio a))) = Constant (PrimNum (NDouble $ fromRational a))
          go x = x

--promoteRatiosMaybe :: MonadReader ModeInfo m => PassT m Prim Prim
--promoteRatiosMaybe = fromKleisli $ proc 
