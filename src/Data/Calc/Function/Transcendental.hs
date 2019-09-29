{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Transcendental where

import Data.Calc.Function.Type
import Data.Calc.Function.Shape
import Data.Calc.Expr
import Data.Calc.Number
import Data.Calc.Mode
import Data.Calc.Coerce

import Data.Ratio
import Control.Monad.Reader
import Control.Monad.Morph

flog :: MonadReader ModeInfo m => Function m
flog = function "ln" f `withDeriv` inOneVar (\x -> pure $ Compound "/" [Constant (PrimNum 1), x])
                       `withShape` always Scalar
    where f = hoist alwaysInexact $ do
            [Constant (PrimNum x)] <- ask
            return . Constant . PrimNum $ log x

fexp :: MonadReader ModeInfo m => Function m
fexp = function "exp" f `withDeriv` inOneVar (\x -> pure $ Compound "exp" [x])
    where f = hoist alwaysInexact $ do
            [Constant (PrimNum x)] <- ask
            return . Constant . PrimNum $ exp x

fsqrt :: MonadReader ModeInfo m => Function m
fsqrt = function "sqrt" f `withDeriv` inOneVar (\x -> pure $ Compound "/" [
                                                       Constant (PrimNum (NRatio $ 1 % 2)),
                                                       Compound "sqrt" [x]
                                                      ])
                          `withShape` always Scalar
    where f = do
            [Constant (PrimNum x)] <- ask
            result <-
                case x of
                  NRatio a
                      | n <- numerator a
                      , d <- denominator a
                      , n' <- fromInteger n :: Double
                      , d' <- fromInteger d :: Double
                      , isPerfectSquare n && isPerfectSquare d ->
                          return . NRatio $ round (sqrt n') % round (sqrt d')
                  a | Just n <- coerceToInt a
                    , n' <- fromInteger n :: Double
                    , isPerfectSquare n -> return . NRatio $ round (sqrt n') % 1
                  a -> hoist alwaysInexact . return $ sqrt a
            return . Constant . PrimNum $ result
          isPerfectSquare n =
              let m = fromInteger n ** 0.5 :: Double
              in m == fromInteger (round m)
