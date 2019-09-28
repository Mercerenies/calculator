{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Arithmetic where

import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Number
import Data.Calc.Mode

import Data.Complex
import Control.Monad.Reader

-- TODO Should we have abs on vectors check to make sure they're one-dimensional first?
fabs :: MonadReader ModeInfo m => Function m
fabs = function "abs" f
    where f = do
            [x] <- ask
            case x of
              Constant (PrimNum (NComplex (a :+ b))) ->
                  pure $ Compound "sqrt" [Constant . PrimNum . NDouble $ a ** 2 + b ** 2]
              Constant (PrimNum x') ->
                  pure $ Constant (PrimNum (abs x'))
              Compound "vector" xs ->
                  pure $ Compound "sqrt" [Compound "+" (map abssqr xs)]
              _ -> fail "cannot find absolute value of unknown expression"
          abssqr x = Compound "^" [Compound "abs" [x], Constant (PrimNum 2)]
