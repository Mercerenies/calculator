{-# LANGUAGE FlexibleContexts, PatternGuards, NamedFieldPuns #-}

module Data.Calc.Calculus.Derivative(derivative, derivativeFn) where

import Data.Calc.Expr
import Data.Calc.Function.Type
import Data.Calc.Mode
import Data.Calc.Util

import Control.Monad.Reader
import Data.Map(Map)
import qualified Data.Map as Map

derivative :: MonadReader ModeInfo m => Map String (Function m) -> String -> Expr Prim ->
              m (Expr Prim)
derivative fns t expr = go expr
    where go (Constant (PrimNum _)) = pure $ Constant (PrimNum 0)
          go (Constant (PrimVar t')) = pure $ Constant (PrimNum $ if t == t' then 1 else 0)
          go (Compound "+" xs) = Compound "+" <$> mapM go xs
          go (Compound "-" [a, b]) = Compound "-" <$> mapM go [a, b]
          go (Compound "*" xs) =
              (Compound "+" . fmap (Compound "*")) <$> duplicateApplyM go xs
          go (Compound "/" [a, b]) = do
            a' <- go a
            b' <- go b
            return $ Compound "/" [
                        Compound "-" [Compound "*" [a', b], Compound "*" [a, b']],
                        Compound "^" [b, Constant $ PrimNum 2]
                       ]
          -- Little known "full power" rule:
          -- (a^b)' = a^b (a' b / a + b' ln(a))
          go (Compound "^" [a, b]) = do
            a' <- go a
            b' <- go b
            return $ Compound "*" [
                        Compound "^" [a, b],
                        Compound "+" [
                         Compound "/" [Compound "*" [a', b], a],
                         Compound "*" [b', Compound "ln" [a]]
                        ]
                       ]
          -- Built-in functions
          go (Compound s xs)
              | Just (Function { fnDerivative }) <- Map.lookup s fns = do
                  let op (n, x) = do
                        inner <- fnDerivative n `appFn` xs
                        outer <- go x
                        return $ (\a -> Compound "*" [a, outer]) <$> inner
                  args <- fmap sequence . mapM op $ zip [0..] xs
                  case args of
                    Nothing -> return $ Compound "D" [Compound s xs, Constant (PrimVar t)]
                    Just args' -> return $ Compound "+" args'
          -- TODO The rest of the rules
          go expr' = pure $ Compound "D" [expr', Constant (PrimVar t)]

-- TODO Permit us to take multiple derivatives at once by passing
-- multiple vars as a list
derivativeFn :: MonadReader ModeInfo m => Map String (Function m) -> Function m
derivativeFn fns = function "D" go
    where go = do
            [expr, Constant (PrimVar t)] <- ask
            lift . lift $ derivative fns t expr
