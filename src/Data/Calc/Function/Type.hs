{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Type(FunctionType, Function(..), functionSynonym, function,
                               withDeriv, inOneVar,
                               simpleUnaryFn, simpleBinaryFn,
                               appFn) where

import Data.Calc.Expr
--import Data.Calc.Mode
import Data.Calc.Number

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Function

type FunctionMonad m = ReaderT [Expr Prim] (MaybeT m)

type FunctionType m = FunctionMonad m (Expr Prim)

--type DerivativeType = (forall m. MonadReader ModeInfo m =>
--                       (Expr Prim -> m (Expr Prim)) ->
--                       [Expr Prim] -> m (Maybe (Expr Prim)))

data Function m = Function {
      fnName :: String,
      fnImpl :: FunctionType m,
      fnDerivative :: Int -> FunctionType m
    }

functionSynonym :: Monad m => String -> String -> Function m
functionSynonym oldname newname = function oldname (Compound newname <$> ask)

function :: Monad m => String -> FunctionType m -> Function m
function name impl = Function name impl (\_ -> fail "no known derivative")

-- Intended to be used infix
withDeriv :: Function m -> (Int -> FunctionType m) -> Function m
withDeriv f t = f { fnDerivative = t }

inOneVar :: Monad m => (Expr Prim -> m (Expr Prim)) -> (Int -> FunctionType m)
inOneVar f 0 = do
  [a] <- ask
  lift . lift $ f a
inOneVar _ _ = fail "index out of bounds (derivative)"

simpleUnaryFn :: Monad m => (Number -> m Number) -> FunctionType m
simpleUnaryFn fn = do
  [Constant (PrimNum a)] <- ask
  Constant . PrimNum <$> (lift . lift $ fn a)

simpleBinaryFn :: Monad m => (Number -> Number -> m Number) -> FunctionType m
simpleBinaryFn fn = do
  [Constant (PrimNum a), Constant (PrimNum b)] <- ask
  Constant . PrimNum <$> (lift . lift $ fn a b)

appFn :: FunctionMonad m a -> [Expr Prim] -> m (Maybe a)
appFn fn exprs = exprs & runReaderT fn & runMaybeT
