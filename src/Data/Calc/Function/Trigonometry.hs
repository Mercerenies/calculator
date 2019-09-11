{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Trigonometry where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type
import Data.Calc.Unit.Radians

import Control.Monad.Reader

fsin :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fsin = simpleUnaryFn (fmap sin . thetaToRad)

fcos :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fcos = simpleUnaryFn (fmap cos . thetaToRad)

ftan :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
ftan = simpleUnaryFn (fmap tan . thetaToRad)

fasin :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fasin = simpleUnaryFn (radToTheta . asin)

facos :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
facos = simpleUnaryFn (radToTheta . acos)

fatan :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fatan = simpleUnaryFn (radToTheta . atan)

fsinh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fsinh = simpleUnaryFn (fmap sinh . thetaToRad)

fcosh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fcosh = simpleUnaryFn (fmap cosh . thetaToRad)

ftanh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
ftanh = simpleUnaryFn (fmap tanh . thetaToRad)

fasinh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fasinh = simpleUnaryFn (radToTheta . asinh)

facosh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
facosh = simpleUnaryFn (radToTheta . acosh)

fatanh :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fatanh = simpleUnaryFn (radToTheta . atanh)
