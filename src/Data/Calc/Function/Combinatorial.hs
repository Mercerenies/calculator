
module Data.Calc.Function.Combinatorial(ffact, fdfact, fncr, fnpr, fgcd, flcm) where

import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Number

import Data.Ratio

-- TODO Factorial can be extended to gamma function on complexes. Some
-- of these others may be extendable to at least reals somehow

ffact :: Applicative m => Function m
ffact = function "fact" f
    where f [Constant (PrimNum x)] = pure . fmap (Constant . PrimNum) $ fact x
          f _ = pure Nothing

fact :: Number -> Maybe Number
fact x = case x of
           (NRatio y)
               | denominator y == 1 && numerator y >= 0 ->
                   Just . fromInteger $ product [1..numerator y]
           _ -> Nothing

fdfact :: Applicative m => Function m
fdfact = function "dfact" f
    where f [Constant (PrimNum x)] = pure . fmap (Constant . PrimNum) $ dfact x
          f _ = pure Nothing

dfact :: Number -> Maybe Number
dfact x = case x of
            (NRatio y)
                | denominator y == 1 && numerator y >= 0 ->
                    Just . fromInteger . product $ takeWhile (> 0) [numerator y, numerator y - 2 ..]
            _ -> Nothing

fncr :: Applicative m => Function m
fncr = function "nCr" f
    where f [Constant (PrimNum x), Constant (PrimNum y)] = pure . fmap (Constant . PrimNum) $ ncr x y
          f _ = pure Nothing

ncr :: Number -> Number -> Maybe Number
ncr (NRatio x) (NRatio y)
        | denominator x == 1 && denominator y == 1 && numerator y >= 0 =
            let x' = numerator x
                y' = numerator y
            in Just . NRatio $ product [x'-y'+1..x'] % product [1..y']
ncr _ _ = Nothing

fnpr :: Applicative m => Function m
fnpr = function "nPr" f
    where f [Constant (PrimNum x), Constant (PrimNum y)] = pure . fmap (Constant . PrimNum) $ npr x y
          f _ = pure Nothing

npr :: Number -> Number -> Maybe Number
npr (NRatio x) (NRatio y)
        | denominator x == 1 && denominator y == 1 && numerator y >= 0 =
            let x' = numerator x
                y' = numerator y
            in Just . NRatio $ product [x'-y'+1..x'] % 1
npr _ _ = Nothing

fgcd :: Applicative m => Function m
fgcd = function "gcd" f
    where f [Constant (PrimNum x), Constant (PrimNum y)] = pure . fmap (Constant . PrimNum) $ gcd' x y
          f _ = pure Nothing
          gcd' (NRatio x) (NRatio y)
              | denominator x == 1 && denominator y == 1 =
                  Just . NRatio $ gcd (numerator x) (numerator y) % 1
          gcd' _ _ = Nothing

flcm :: Applicative m => Function m
flcm = function "lcm" f
    where f [Constant (PrimNum x), Constant (PrimNum y)] = pure . fmap (Constant . PrimNum) $ lcm' x y
          f _ = pure Nothing
          lcm' (NRatio x) (NRatio y)
              | denominator x == 1 && denominator y == 1 =
                  Just . NRatio $ lcm (numerator x) (numerator y) % 1
          lcm' _ _ = Nothing
