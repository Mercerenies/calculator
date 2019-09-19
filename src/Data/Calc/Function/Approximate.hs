{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Approximate(promoteRatios, approximately, approx) where

import Data.Calc.Function.Type
import Data.Calc.Normalize
import Data.Calc.Pass
import Data.Calc.Mode

import Prelude hiding ((.), id)
import Control.Monad.Reader
import Data.Map(Map)

-- This module provides the implementation of the N(...) function,
-- which promotes all rationals to floats and forces evaluation of
-- symbolic quantities such as sqrt(2).

-- NOTE: Currently, only the former behavior is implemented. Once we
-- have a notion of symbolic quantities and teach the simplifier how
-- to handle them, we'll implement the override here.

approximately :: MonadReader ModeInfo m => Map String (Function m) -> FunctionType m
approximately fns = do
  [expr] <- ask
  lift . lift $ local (\mode -> mode { exactnessMode = Floating}) $ runPassTDM (basicPass fns) expr

approx :: MonadReader ModeInfo m => Map String (Function m) -> Function m
approx fns = function "N" (approximately fns)
