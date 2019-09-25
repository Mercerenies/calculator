{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Unit.Radians where

import Data.Calc.Mode
import Data.Calc.Expr
import Data.Calc.Unit.Type
import Data.Calc.Unit.Table

import Control.Monad.Reader

-- TODO Use Data.Calc.Unit.Table for these

angularModeToUnit :: AngularMode -> Unit (Expr Prim) (Expr Prim)
angularModeToUnit Degrees = snd degrees
angularModeToUnit Radians = snd radians

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

thetaToRadFactorSym :: MonadReader ModeInfo m => m (Expr Prim)
thetaToRadFactorSym = asks angularMode >>= \case
                      Radians -> return $ Constant (PrimNum 1)
                      Degrees -> return $ Compound "/" [Constant (PrimVar "pi"),
                                                        Constant (PrimNum 180)]

radToThetaFactorSym :: MonadReader ModeInfo m => m (Expr Prim)
radToThetaFactorSym = (\x -> Compound "/" [Constant (PrimNum 1), x]) <$> thetaToRadFactorSym
