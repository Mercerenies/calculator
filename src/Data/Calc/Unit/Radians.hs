{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Unit.Radians where

import Data.Calc.Mode

import Control.Monad.Reader

thetaToRad :: (Floating a, MonadReader ModeInfo m) => a -> m a
thetaToRad x = fmap (x *) thetaToRadFactor

radToTheta :: (Floating a, MonadReader ModeInfo m) => a -> m a
radToTheta x = fmap (* x) radToThetaFactor

thetaToRadFactor :: (Floating a, MonadReader ModeInfo m) => m a
thetaToRadFactor = asks angularMode >>= \case
                   Radians -> return 1
                   Degrees -> return (pi / 180)

radToThetaFactor :: (Floating a, MonadReader ModeInfo m) => m a
radToThetaFactor = recip <$> thetaToRadFactor
