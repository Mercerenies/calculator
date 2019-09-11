{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Transcendental where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type

import Control.Monad.Reader

flog :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
flog = simpleUnaryFn (pure . log) -- TODO Exact result if arg == 1

fexp :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fexp = simpleUnaryFn (pure . exp) -- TODO Exact result if arg == 0
