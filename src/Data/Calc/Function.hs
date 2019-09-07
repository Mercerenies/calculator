{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, LambdaCase #-}

module Data.Calc.Function where

import Data.Calc.Expr
import Data.Calc.Mode

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

fsin :: MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))
fsin [Constant (PrimNum a)] = (Just . Constant . PrimNum . sin) <$> thetaToRad a
fsin _ = pure Nothing

stdBuiltins :: Map String Function
stdBuiltins = Map.fromList [
            ("sin", Function fsin)
           ]

applyTo :: MonadReader ModeInfo m => Map String Function -> String -> [Expr Prim] -> m (Expr Prim)
applyTo m s args = case Map.lookup s m of
                     Nothing  -> pure $ Compound s args
                     Just (Function fn) -> fn args >>= \case
                                           Nothing -> pure (Compound s args)
                                           Just x  -> pure x

applyToStd :: MonadReader ModeInfo m => String -> [Expr Prim] -> m (Expr Prim)
applyToStd = applyTo stdBuiltins
