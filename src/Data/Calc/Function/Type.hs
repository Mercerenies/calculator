{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Type(FunctionType, Function(..), functionSynonym, function,
                               withDeriv, inOneVar,
                               simpleUnaryFn, simpleBinaryFn) where

import Data.Calc.Expr
--import Data.Calc.Mode
import Data.Calc.Number

--import Control.Monad.Reader

type FunctionType m = [Expr Prim] -> m (Maybe (Expr Prim))

--type DerivativeType = (forall m. MonadReader ModeInfo m =>
--                       (Expr Prim -> m (Expr Prim)) ->
--                       [Expr Prim] -> m (Maybe (Expr Prim)))

data Function m = Function {
      fnName :: String,
      fnImpl :: FunctionType m,
      fnDerivative :: Int -> FunctionType m
    }

functionSynonym :: Applicative m => String -> String -> Function m
functionSynonym oldname newname = function oldname (pure . Just . Compound newname)

function :: Applicative m => String -> FunctionType m -> Function m
function name impl = Function name impl (\_ _ -> pure Nothing) -- By default, no known derivative

-- Intended to be used infix
withDeriv :: Function m -> (Int -> FunctionType m) -> Function m
withDeriv f t = f { fnDerivative = t }

inOneVar :: Applicative m => (Expr Prim -> m (Expr Prim)) -> (Int -> FunctionType m)
inOneVar f 0 [a] = Just <$> f a
inOneVar _ _ _ = pure Nothing

simpleUnaryFn :: Applicative m =>
                 (Number -> m Number) ->
                 ([Expr Prim] -> m (Maybe (Expr Prim)))
simpleUnaryFn fn [Constant (PrimNum a)] = (Just . Constant . PrimNum) <$> fn a
simpleUnaryFn _ _ = pure Nothing

simpleBinaryFn :: Applicative m =>
                  (Number -> Number -> m Number) ->
                  ([Expr Prim] -> m (Maybe (Expr Prim)))
simpleBinaryFn fn [Constant (PrimNum a), Constant (PrimNum b)] = (Just . Constant . PrimNum) <$> fn a b
simpleBinaryFn _ _ = pure Nothing
