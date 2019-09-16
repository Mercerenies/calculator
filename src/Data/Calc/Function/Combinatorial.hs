{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Combinatorial(ffact, fncr) where

import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Number

import Data.Ratio

-- TODO Factorial can be extended to gamma function on complexes. Some
-- of these others may be extendable to at least reals somehow

ffact :: Function
ffact = function "fact" f
    where f :: FunctionType
          f [Constant (PrimNum x)] = pure . fmap (Constant . PrimNum) $ fact x
          f _ = pure Nothing

fact :: Number -> Maybe Number
fact x = case x of
           (NRatio y)
               | denominator y == 1 && numerator y >= 0 ->
                   Just . fromInteger $ product [1..numerator y]
           _ -> Nothing

fncr :: Function
fncr = function "nCr" f
    where f :: FunctionType
          f [Constant (PrimNum x), Constant (PrimNum y)] = pure . fmap (Constant . PrimNum) $ ncr x y
          f _ = pure Nothing

ncr :: Number -> Number -> Maybe Number
ncr (NRatio x) (NRatio y)
        | denominator x == 1 && denominator y == 1 && numerator y >= 0 =
            let x' = numerator x
                y' = numerator y
            in Just . NRatio $ product [x'-y'+1..x'] % product [1..y']
ncr _ _ = Nothing
