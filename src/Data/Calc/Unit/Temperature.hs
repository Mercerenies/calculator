
module Data.Calc.Unit.Temperature where

-- So we need a special module to deal with temperature shenanigans.
-- Temperatures are weird because (aside from Kelvins) they have a
-- translation component. If we use the usual unit conversion
-- functions, they'll treat 0 degC the same as 0 degF the same as 0 K,
-- which is fine if the temperatures are relative but not if they're
-- absolute. If we want to do absolute temperature conversions, we
-- need this module.


