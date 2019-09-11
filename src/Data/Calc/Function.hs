{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, LambdaCase #-}

module Data.Calc.Function(Function(..), functionSynonym,
                          stdBuiltins,
                          applyTo, applyToStd) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type
import Data.Calc.Function.Approximate
import Data.Calc.Function.Trigonometry
import Data.Calc.Function.Transcendental

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader

stdBuiltins :: Map String Function
stdBuiltins = Map.fromList [
            ("sin", Function fsin),
            ("cos", Function fcos),
            ("tan", Function ftan),
            ("asin", Function fasin),
            ("acos", Function facos),
            ("atan", Function fatan),
            ("sinh", Function fsinh),
            ("cosh", Function fcosh),
            ("tanh", Function ftanh),
            ("asinh", Function fasinh),
            ("acosh", Function facosh),
            ("atanh", Function fatanh),
            ("log", functionSynonym "ln"),
            ("ln", Function flog),
            ("exp", Function fexp),
            ("N", approx)
           ]

applyTo :: MonadReader ModeInfo m => Map String Function -> String -> [Expr Prim] -> m (Expr Prim)
applyTo m s args = case Map.lookup s m of
                     Nothing  -> pure $ Compound s args
                     Just (Function fn) -> fn args >>= \case
                                           Nothing -> pure (Compound s args)
                                           Just x  -> pure x

applyToStd :: MonadReader ModeInfo m => String -> [Expr Prim] -> m (Expr Prim)
applyToStd = applyTo stdBuiltins
