{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Data.Calc.Function.Type(FunctionType, Function(..), functionSynonym, function,
                               simpleUnaryFn) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Number

import Control.Monad.Reader

type FunctionType = (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))

data Function = Function {
      fnName :: String,
      fnImpl :: FunctionType
    }

functionSynonym :: String -> String -> Function
functionSynonym oldname newname = function oldname (pure . Just . Compound newname)

function :: String -> FunctionType -> Function
function = Function

simpleUnaryFn :: (forall m. MonadReader ModeInfo m => Number -> m Number) ->
                 (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))
simpleUnaryFn fn [Constant (PrimNum a)] = (Just . Constant . PrimNum) <$> fn a
simpleUnaryFn _ _ = pure Nothing
