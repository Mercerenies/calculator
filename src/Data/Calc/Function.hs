{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, LambdaCase #-}

module Data.Calc.Function where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Number

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader

newtype Function where
    Function :: (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))) -> Function

thetaToRad :: (Floating a, MonadReader ModeInfo m) => a -> m a
thetaToRad x = asks angularMode >>= \case
               Radians -> return x
               Degrees -> return $ (pi / 180) * x

radToTheta :: (Floating a, MonadReader ModeInfo m) => a -> m a
radToTheta x = asks angularMode >>= \case
               Radians -> return x
               Degrees -> return $ (180 / pi) * x

simpleUnaryFn :: (forall m. MonadReader ModeInfo m => Number -> m Number) ->
                 (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))
simpleUnaryFn fn [Constant (PrimNum a)] = (Just . Constant . PrimNum) <$> fn a
simpleUnaryFn _ _ = pure Nothing

functionSynonym :: String -> Function
functionSynonym newname = Function (pure . Just . Compound newname)

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

flog :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
flog = simpleUnaryFn (pure . log) -- TODO Exact result if arg == 1

fexp :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fexp = simpleUnaryFn (pure . exp) -- TODO Exact result if arg == 0

stdBuiltins :: Map String Function
stdBuiltins = Map.fromList [
            ("sin", Function fsin),
            ("cos", Function fcos),
            ("tan", Function ftan),
            ("asin", Function fasin),
            ("acos", Function facos),
            ("atan", Function fatan),
            ("sinh", Function fsinh),
            ("cosh", Function fcosh),
            ("tanh", Function ftanh),
            ("asinh", Function fasinh),
            ("acosh", Function facosh),
            ("atanh", Function fatanh),
            ("log", functionSynonym "ln"),
            ("ln", Function flog),
            ("exp", Function fexp)
           ]

applyTo :: MonadReader ModeInfo m => Map String Function -> String -> [Expr Prim] -> m (Expr Prim)
applyTo m s args = case Map.lookup s m of
                     Nothing  -> pure $ Compound s args
                     Just (Function fn) -> fn args >>= \case
                                           Nothing -> pure (Compound s args)
                                           Just x  -> pure x

applyToStd :: MonadReader ModeInfo m => String -> [Expr Prim] -> m (Expr Prim)
applyToStd = applyTo stdBuiltins
