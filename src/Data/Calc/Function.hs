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
import Data.Calc.Calculus.Derivative

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Arrow

compileFns :: [Function] -> Map String Function
compileFns = fmap (fnName &&& id) >>> Map.fromList

stdBuiltins :: Map String Function
stdBuiltins = compileFns [
            Trig.fsin, Trig.fcos, Trig.ftan,
            Trig.fasin, Trig.facos, Trig.fatan,
            Trig.fsinh, Trig.fcosh, Trig.ftanh,
            Trig.fasinh, Trig.facosh, Trig.fatanh,
            functionSynonym "log" "ln",
            Trans.flog, Trans.fexp,
            approx,
            derivativeFn
           ]

applyTo :: MonadReader ModeInfo m => Map String Function -> String -> [Expr Prim] -> m (Expr Prim)
applyTo m s args = case Map.lookup s m of
                     Nothing  -> pure $ Compound s args
                     Just (Function { fnImpl = fn }) -> fn args >>= \case
                                                        Nothing -> pure (Compound s args)
                                                        Just x  -> pure x

applyToStd :: MonadReader ModeInfo m => String -> [Expr Prim] -> m (Expr Prim)
applyToStd = applyTo stdBuiltins
