{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Data.Calc.Function.Type(FunctionType, Function(..), functionSynonym, function,
                               withDeriv, inOneVar,
                               simpleUnaryFn) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Number

import Control.Monad.Reader

type FunctionType = (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))

--type DerivativeType = (forall m. MonadReader ModeInfo m =>
--                       (Expr Prim -> m (Expr Prim)) ->
--                       [Expr Prim] -> m (Maybe (Expr Prim)))

data Function = Function {
      fnName :: String,
      fnImpl :: FunctionType,
      fnDerivative :: Int -> FunctionType
    }

functionSynonym :: String -> String -> Function
functionSynonym oldname newname = function oldname (pure . Just . Compound newname)

function :: String -> FunctionType -> Function
function name impl = Function name impl (\_ _ -> pure Nothing) -- By default, no known derivative

-- Intended to be used infix
withDeriv :: Function -> (Int -> FunctionType) -> Function
withDeriv f t = f { fnDerivative = t }

inOneVar :: (forall m. MonadReader ModeInfo m => Expr Prim -> m (Expr Prim)) -> (Int -> FunctionType)
inOneVar f 0 [a] = Just <$> f a
inOneVar _ _ _ = pure Nothing

simpleUnaryFn :: (forall m. MonadReader ModeInfo m => Number -> m Number) ->
                 (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim)))
simpleUnaryFn fn [Constant (PrimNum a)] = (Just . Constant . PrimNum) <$> fn a
simpleUnaryFn _ _ = pure Nothing
