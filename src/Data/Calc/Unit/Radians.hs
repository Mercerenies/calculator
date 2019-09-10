{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Unit.Radians where

import Data.Calc.Mode

import Control.Monad.Reader

thetaToRad :: (Floating a, MonadReader ModeInfo m) => a -> m a
thetaToRad x = asks angularMode >>= \case
               Radians -> return x
               Degrees -> return $ (pi / 180) * x

radToTheta :: (Floating a, MonadReader ModeInfo m) => a -> m a
radToTheta x = asks angularMode >>= \case
               Radians -> return x
               Degrees -> return $ (180 / pi) * x
