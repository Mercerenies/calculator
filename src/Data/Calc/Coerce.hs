
module Data.Calc.Coerce where

import Data.Calc.Number

import Prelude hiding (fail)
import Control.Monad.Fail
import Data.Complex
import Data.Ratio

coerceToInt :: MonadFail m => Number -> m Integer
coerceToInt (NRatio a)   | denominator a == 1         = pure (numerator a)
coerceToInt (NDouble a)  | a == fromInteger (round a) = pure (round a)
coerceToInt (NComplex (a :+ 0)) = coerceToInt (NDouble a)
coerceToInt _ = fail "cannot coerce to int"
