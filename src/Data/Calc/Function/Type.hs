{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs #-}

module Data.Calc.Function.Type(Function(..), functionSynonym,
                               simpleUnaryFn) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Number

import Control.Monad.Reader

newtype Function where
    Function :: (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))) -> Function

functionSynonym :: String -> Function
functionSynonym newname = Function (pure . Just . Compound newname)

simpleUnaryFn :: (forall m. MonadReader ModeInfo m => Number -> m Number) ->
                 (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))
simpleUnaryFn fn [Constant (PrimNum a)] = (Just . Constant . PrimNum) <$> fn a
simpleUnaryFn _ _ = pure Nothing
