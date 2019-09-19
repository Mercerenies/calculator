{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Approximate(promoteRatios, approximately, approx) where

import Data.Calc.Expr
import Data.Calc.Function.Type
import Data.Calc.Pass
import Data.Calc.Mode
import Data.Calc.Number

import Prelude hiding ((.), id)
import Control.Monad.Reader

-- This module provides the implementation of the N(...) function,
-- which promotes all rationals to floats and forces evaluation of
-- symbolic quantities such as sqrt(2).

-- NOTE: Currently, only the former behavior is implemented. Once we
-- have a notion of symbolic quantities and teach the simplifier how
-- to handle them, we'll implement the override here.

promoteRatios :: Monad m => PassT m Prim Prim
promoteRatios = pass go
    where go (Constant (PrimNum (NRatio a))) = Constant (PrimNum (NDouble $ fromRational a))
          go x = x

approximately :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
approximately [expr] = pure . Just $ runPassOnceTD promoteRatios expr
approximately _ = pure Nothing

approx :: MonadReader ModeInfo m => Function m
approx = function "N" approximately
