{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Transcendental where

import Data.Calc.Function.Type
import Data.Calc.Expr

flog :: Function
flog = function "ln" f `withDeriv` inOneVar (\x -> pure $ Compound "/" [Constant (PrimNum 1), x])
    where f :: FunctionType
          f = simpleUnaryFn (pure . log) -- TODO Exact result if arg == 1

fexp :: Function
fexp = function "exp" f `withDeriv` inOneVar (\x -> pure $ Compound "exp" [x])
    where f :: FunctionType
          f = simpleUnaryFn (pure . exp) -- TODO Exact result if arg == 0
