{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Transcendental where

import Data.Calc.Function.Type

flog :: Function
flog = function "ln" f
    where f :: FunctionType
          f = simpleUnaryFn (pure . log) -- TODO Exact result if arg == 1

fexp :: Function
fexp = function "exp" f
    where f :: FunctionType
          f = simpleUnaryFn (pure . exp) -- TODO Exact result if arg == 0