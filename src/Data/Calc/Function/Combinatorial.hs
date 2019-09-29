
module Data.Calc.Function.Combinatorial(ffact, fdfact, fncr, fnpr, fgcd, flcm) where

import Data.Calc.Function.Type
import Data.Calc.Function.Shape
import Data.Calc.Expr
import Data.Calc.Number
import Data.Calc.Coerce

import Data.Ratio
import Control.Monad.Reader

-- TODO Factorial can be extended to gamma function on complexes. Some
-- of these others may be extendable to at least reals somehow

ffact :: Monad m => Function m
ffact = function "fact" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x)] <- ask
            x' <- coerceToInt x
            guard $ x' >= 0
            return . Constant . PrimNum . NRatio $ fact x' % 1
          fact x = product [1..x]

fdfact :: Monad m => Function m
fdfact = function "dfact" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x)] <- ask
            x' <- coerceToInt x
            guard $ x' >= 0
            return . Constant . PrimNum . NRatio $ dfact x' % 1
          dfact x = fromInteger . product $ takeWhile (> 0) [x, x - 2 ..]

fncr :: Monad m => Function m
fncr = function "nCr" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x), Constant (PrimNum y)] <- ask
            x' <- coerceToInt x
            y' <- coerceToInt y
            guard $ y' >= 0
            return . Constant . PrimNum . NRatio $ ncr x' y'
          ncr x y = product [x-y+1..x] % product [1..y]

fnpr :: Monad m => Function m
fnpr = function "nPr" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x), Constant (PrimNum y)] <- ask
            x' <- coerceToInt x
            y' <- coerceToInt y
            guard $ y' >= 0
            return . Constant . PrimNum . NRatio $ npr x' y'
          npr x y = product [x-y+1..x] % 1

fgcd :: Monad m => Function m
fgcd = function "gcd" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x), Constant (PrimNum y)] <- ask
            x' <- coerceToInt x
            y' <- coerceToInt y
            return . Constant . PrimNum . NRatio $ gcd x' y' % 1

flcm :: Monad m => Function m
flcm = function "lcm" f `withShape` always Scalar
    where f = do
            [Constant (PrimNum x), Constant (PrimNum y)] <- ask
            x' <- coerceToInt x
            y' <- coerceToInt y
            return . Constant . PrimNum . NRatio $ lcm x' y' % 1
