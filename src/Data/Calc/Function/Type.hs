{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Function.Type(FunctionType, FunctionShape, Function(..), applyTo,
                               functionSynonym, function,
                               withDeriv, withShape, inOneVar,
                               simpleUnaryFn, simpleBinaryFn,
                               alwaysInexact,
                               appFn) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Number
import Data.Calc.Shape.Type

import Prelude hiding (fail)
import Control.Monad.Reader hiding (fail)
import Control.Monad.Trans.Maybe
import Control.Monad.Fail
import Data.Function
import Data.Map(Map)
import qualified Data.Map as Map

type FunctionMonad m = ReaderT [Expr Prim] (MaybeT m)

type FunctionType m = FunctionMonad m (Expr Prim)

type FunctionShape = (Expr Prim -> Shape) -> [Expr Prim] -> Shape

data Function m = Function {
      fnName :: String,
      fnImpl :: FunctionType m,
      fnDerivative :: Int -> FunctionType m,
      fnShape :: FunctionShape
    }

applyTo :: MonadReader ModeInfo m => Map String (Function m) -> String -> [Expr Prim] -> m (Expr Prim)
applyTo m s args = case Map.lookup s m of
                     Nothing  -> pure $ Compound s args
                     Just (Function { fnImpl = fn }) -> fn `appFn` args >>= \case
                                                        Nothing -> pure (Compound s args)
                                                        Just x  -> pure x

-- TODO Should we make this smarter so it inherits derivative and shape properties?
functionSynonym :: Monad m => String -> String -> Function m
functionSynonym oldname newname = function oldname (Compound newname <$> ask)

function :: Monad m => String -> FunctionType m -> Function m
function name impl = Function name impl (\_ -> fail "no known derivative") (\_ _ -> Unknown)

-- Intended to be used infix
withDeriv :: Function m -> (Int -> FunctionType m) -> Function m
withDeriv f t = f { fnDerivative = t }

-- Intended to be used infix
withShape :: Function m -> FunctionShape -> Function m
withShape f t = f { fnShape = t }

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

alwaysInexact :: (MonadReader ModeInfo m, MonadPlus m) => m a -> m a
alwaysInexact m = do
  exactness <- asks exactnessMode
  guard (exactness < Symbolic)
  m

appFn :: FunctionMonad m a -> [Expr Prim] -> m (Maybe a)
appFn fn exprs = exprs & runReaderT fn & runMaybeT
