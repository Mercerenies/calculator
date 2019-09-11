{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, LambdaCase #-}

module Data.Calc.Function(Function(..), functionSynonym,
                          stdBuiltins,
                          applyTo, applyToStd) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type
import Data.Calc.Function.Approximate
import qualified Data.Calc.Function.Trigonometry as Trig
import qualified Data.Calc.Function.Transcendental as Trans

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader

stdBuiltins :: Map String Function
stdBuiltins = Map.fromList [
            ("sin", Trig.fsin),
            ("cos", Trig.fcos),
            ("tan", Trig.ftan),
            ("asin", Trig.fasin),
            ("acos", Trig.facos),
            ("atan", Trig.fatan),
            ("sinh", Trig.fsinh),
            ("cosh", Trig.fcosh),
            ("tanh", Trig.ftanh),
            ("asinh", Trig.fasinh),
            ("acosh", Trig.facosh),
            ("atanh", Trig.fatanh),
            ("log", functionSynonym "ln"),
            ("ln", Trans.flog),
            ("exp", Trans.fexp),
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
