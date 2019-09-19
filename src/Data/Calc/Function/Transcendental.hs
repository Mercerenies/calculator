{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Transcendental where

import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Number

import Data.Ratio
import Control.Monad.Reader

flog :: Monad m => Function m
flog = function "ln" f `withDeriv` inOneVar (\x -> pure $ Compound "/" [Constant (PrimNum 1), x])
    where f = do
            [Constant (PrimNum x)] <- ask
            return . Constant . PrimNum $ log x

fexp :: Monad m => Function m
fexp = function "exp" f `withDeriv` inOneVar (\x -> pure $ Compound "exp" [x])
    where f = do
            [Constant (PrimNum x)] <- ask
            return . Constant . PrimNum $ exp x

fsqrt :: Monad m => Function m
fsqrt = function "sqrt" f `withDeriv` inOneVar (\x -> pure $ Compound "/" [
                                                       Constant (PrimNum (NRatio $ 1 % 2)),
                                                       Compound "sqrt" [x]
                                                      ])
    where f = do
            [Constant (PrimNum x)] <- ask
            return . Constant . PrimNum $ sqrt x
