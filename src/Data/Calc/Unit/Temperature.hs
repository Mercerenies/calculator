
module Data.Calc.Unit.Temperature(TemperatureUnit(..),
                                  kelvins, celsius, fahrenheit,
                                  tempConvert, tempToRelativeUnit) where

import Data.Calc.Expr
import Data.Calc.Unit.Type
import qualified Data.Calc.Unit.Dimension as Dim

import Data.Ratio

-- So we need a special module to deal with temperature shenanigans.
-- Temperatures are weird because (aside from Kelvins) they have a
-- translation component. If we use the usual unit conversion
-- functions, they'll treat 0 degC the same as 0 degF the same as 0 K,
-- which is fine if the temperatures are relative but not if they're
-- absolute. If we want to do absolute temperature conversions, we
-- need this module.

-- A temperature consists of a scaling followed by a translation.
-- These are the necessary transformations (in that order) to go from
-- Kelvins TO the temperature.
data TemperatureUnit = TemperatureUnit {
      tempScale :: Expr Prim,
      tempTranslate :: Expr Prim
    }

kelvins, celsius, fahrenheit :: (String, TemperatureUnit)
kelvins = ("K", TemperatureUnit (Constant $ PrimNum 1)
                                (Constant $ PrimNum 1))
celsius = ("degC", TemperatureUnit (Constant $ PrimNum 1)
                                   (Constant $ PrimNum (fromRational $ - 5463 % 20)))
fahrenheit = ("degF", TemperatureUnit (Constant $ PrimNum (fromRational $ 9 % 5))
                                      (Constant $ PrimNum (fromRational $ - 45967 % 100)))

-- Temperatures don't commute with other units, so this function is
-- for internal use only since I'll only be using it here on
-- one-dimensional (temperature) units.
unsafeTempToUnit :: TemperatureUnit -> Unit (Expr Prim) (Expr Prim)
unsafeTempToUnit (TemperatureUnit s t) = Unit Dim.temperature toBase fromBase
    where toBase x = Compound "/" [Compound "-" [x, t], s]
          fromBase x = Compound "+" [Compound "*" [s, x], t]

-- There are no dimensions to check here, so it always succeeds :D
tempConvert :: TemperatureUnit -> TemperatureUnit -> Expr Prim -> Expr Prim
tempConvert u u' = unsafeConvert (unsafeTempToUnit u) (unsafeTempToUnit u')

tempToRelativeUnit :: TemperatureUnit -> Unit (Expr Prim) (Expr Prim)
tempToRelativeUnit (TemperatureUnit s _) = Unit Dim.temperature toBase fromBase
    where toBase x = Compound "/" [x, s]
          fromBase x = Compound "*" [s, x]
