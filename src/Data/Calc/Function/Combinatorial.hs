
module Data.Calc.Function.Combinatorial(ffact, fdfact, fncr, fnpr, fgcd, flcm) where

import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Number

import Data.Ratio
import Control.Monad.Reader

-- TODO Factorial can be extended to gamma function on complexes. Some
-- of these others may be extendable to at least reals somehow

ffact :: Monad m => Function m
ffact = function "fact" f
    where f = do
            [Constant (PrimNum (NRatio x))] <- ask
            guard $ denominator x == 1 && numerator x >= 0
            return . Constant . PrimNum . NRatio $ fact (numerator x) % 1
          fact x = product [1..x]

fdfact :: Monad m => Function m
fdfact = function "dfact" f
    where f = do
            [Constant (PrimNum (NRatio x))] <- ask
            guard $ denominator x == 1 && numerator x >= 0
            return . Constant . PrimNum . NRatio $ dfact (numerator x) % 1
          dfact x = fromInteger . product $ takeWhile (> 0) [x, x - 2 ..]

fncr :: Monad m => Function m
fncr = function "nCr" f
    where f = do
            [Constant (PrimNum (NRatio x)), Constant (PrimNum (NRatio y))] <- ask
            guard $ denominator x == 1 && denominator y == 1 && numerator y >= 0
            return . Constant . PrimNum . NRatio $ ncr (numerator x) (numerator y)
          ncr x y = product [x-y+1..x] % product [1..y]

fnpr :: Monad m => Function m
fnpr = function "nPr" f
    where f = do
            [Constant (PrimNum (NRatio x)), Constant (PrimNum (NRatio y))] <- ask
            guard $ denominator x == 1 && denominator y == 1 && numerator y >= 0
            return . Constant . PrimNum . NRatio $ npr (numerator x) (numerator y)
          npr x y = product [x-y+1..x] % 1

fgcd :: Monad m => Function m
fgcd = function "gcd" f
    where f = do
            [Constant (PrimNum (NRatio x)), Constant (PrimNum (NRatio y))] <- ask
            guard $ denominator x == 1 && denominator y == 1
            return . Constant . PrimNum . NRatio $ gcd (numerator x) (numerator y) % 1

flcm :: Monad m => Function m
flcm = function "lcm" f
    where f = do
            [Constant (PrimNum (NRatio x)), Constant (PrimNum (NRatio y))] <- ask
            guard $ denominator x == 1 && denominator y == 1
            return . Constant . PrimNum . NRatio $ lcm (numerator x) (numerator y) % 1
