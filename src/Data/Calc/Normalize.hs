{-# LANGUAGE ScopedTypeVariables #-}

module Data.Calc.Normalize where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Repr
import Data.Calc.Util

import Prelude hiding ((.), id)
import Data.List(sort)
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.State

normalizeNegatives :: ReprInteger a => Pass a a
normalizeNegatives = Pass subtractionPass . Pass negationPass
    where subtractionPass (Compound "-" [a, b]) =
              Compound "+" [a, Compound "*" [reprInteger (-1), b]]
          subtractionPass x = x
          negationPass (Compound "_" [b]) =
              Compound "*" [reprInteger (-1), b]
          negationPass x = x

levelOperators :: [String] -> Pass a a
levelOperators ss = foldr (.) id $ map (Pass . go) ss
    where go str (Compound str' xs)
             | str == str' = Compound str' $ concatMap (flatten str) xs
          go _ x = x
          flatten str (Compound str' xs)
              | str == str' = xs
          flatten _ x = [x]

levelStdOperators :: Pass a a
levelStdOperators = levelOperators ["+", "*"]

simplifyRationals :: Pass a a
simplifyRationals = foldr (.) id $ map Pass [rule1, rule2, rule3]
    where rule1 (Compound "/" [Compound "/" [a, b], c]) = Compound "/" [a, Compound "*" [b, c]]
          rule1 x = x
          rule2 (Compound "/" [a, Compound "/" [b, c]]) = Compound "/" [Compound "*" [a, c], b]
          rule2 x = x
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

collectLikeFactors :: forall a. (ReprInteger a, HasVars a, HasNumbers a, Ord a) => Pass a a
collectLikeFactors = Pass collect
    where collect (Compound "*" xs) = let (res, m) = runState (concat <$> mapM match xs) Map.empty
                                      in Compound "*" (res ++ (map coalesce $ Map.toList m))
          collect x = x
          match :: Expr a -> State (Map a [Expr a]) [Expr a]
          match (Constant a) | isVar a = [] <$ modify (mappendMap a [Constant (reprInteger 1)])
          match (Compound "^" [Constant a, Constant b])
              | isVar a && isNumber b = [] <$ modify (mappendMap a [Constant b])
          match x = pure [x]
          coalesce (a, [x]) | x == Constant (reprInteger 1) = Constant a
          coalesce (a, [x]) = Compound "^" [Constant a, x]
          coalesce (a, es) = Compound "^" [Constant a, Compound "+" es]

collectLikeTerms :: forall a. (ReprInteger a, HasVars a, HasNumbers a, Ord a) => Pass a a
collectLikeTerms = Pass collect
    where collect (Compound "+" xs) = let (res, m) = runState (concat <$> mapM match xs) Map.empty
                                      in Compound "+" (res ++ (map coalesce $ Map.toList m))
          collect x = x
          match :: Expr a -> State (Map a [Expr a]) [Expr a]
          match (Constant a) | isVar a = [] <$ modify (mappendMap a [Constant (reprInteger 1)])
          match (Compound "*" [Constant a, Constant b])
              | isVar a && isNumber b = [] <$ modify (mappendMap a [Constant b])
          match (Compound "*" [Constant a, Constant b])
              | isVar b && isNumber a = [] <$ modify (mappendMap b [Constant a])
          match x = pure [x]
          coalesce (a, [x]) | x == Constant (reprInteger 1) = Constant a
          coalesce (a, [x]) = Compound "*" [x, Constant a]
          coalesce (a, es) = Compound "*" [Compound "+" es, Constant a]

-- TODO I'd like to generalize this to take Pass a a for appropriately typeclassed a.
foldConstants :: Pass Prim Prim
foldConstants = Pass eval
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
evalFunctions :: Pass Prim Prim
evalFunctions = id -- Pass eval -- ////
--    where eval (Compound h xs) = 

flattenSingletons :: [String] -> Pass a a
flattenSingletons ss = foldr (.) id $ map (Pass . go) ss
    where go str (Compound str' [a]) | str == str' = a
          go _ x = x

flattenStdSingletons :: Pass a a
flattenStdSingletons = flattenSingletons ["+", "*"]

sortTermsOf :: Ord a => [String] -> Pass a a
sortTermsOf ss = foldr (.) id $ map (Pass . go) ss
    where go str (Compound str' xs) | str == str' = Compound str' (sort xs)
          go _ x = x

sortTermsOfStd :: Ord a => Pass a a
sortTermsOfStd = sortTermsOf ["+", "*"]
