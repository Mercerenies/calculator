{-# LANGUAGE RecordWildCards #-}

module Data.Calc.Unit.Table(expandSIPrefixes, radians, degrees, table, simpleConvert) where

import Data.Calc.Unit.Type
import Data.Calc.Unit.Parse
import qualified Data.Calc.Unit.Dimension as Dim
import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Util
import Data.Calc.Number

import Prelude hiding (fail)
import Data.Semigroup
import Data.Ratio
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (fail)
import Control.Monad.Fail

-- A lot of the measurements in this file are from the Emacs Calc
-- units table.

multiplyBy :: Expr a -> Expr a -> Expr a
multiplyBy x y = Compound "*" [x, y]

recipOf :: Expr Prim -> Expr Prim
recipOf n = Compound "/" [Constant $ PrimNum 1, n]

unitByFactor :: Dim.Dimension -> Expr Prim -> Unit (Expr Prim) (Expr Prim)
unitByFactor dim factor =
    Unit dim (multiplyBy $ recipOf factor) (multiplyBy factor)

expandSIPrefixes :: (String, Unit (Expr Prim) (Expr Prim)) -> [(String, Unit (Expr Prim) (Expr Prim))]
expandSIPrefixes (name, Unit {..}) = fmap go prefixes
    where go (prefix, n) =
              let n' = Constant . PrimNum $ 10 ^^ n
              in (prefix ++ name, Unit unitDim (multiplyBy n' . unitToBase)
                                               (unitFromBase . multiplyBy (recipOf n')))
          prefixes :: [(String, Integer)]
          prefixes = [("Y",  24),
                      ("Z",  21),
                      ("E",  18),
                      ("P",  15),
                      ("T",  12),
                      ("G",   9),
                      ("M",   6),
                      ("k",   3),
                      ("h",   2),
                      ("D",   1),
                      ("" ,   0),
                      ("d",  -1),
                      ("c",  -2),
                      ("m",  -3),
                      ("u",  -6),
                      ("μ",  -6),
                      ("n",  -9),
                      ("p", -12),
                      ("f", -15),
                      ("a", -18),
                      ("z", -21),
                      ("y", -24)]

radians, degrees,
 meters, inches, feet, yards, miles, astrounits, lightyears, parsecs,
 seconds, seconds', minutes, hours, days, weeks, years,
 liters
    :: (String, Unit (Expr Prim) (Expr Prim))

radians = ("rad", Unit Dim.angle id id)
degrees = ("deg", unitByFactor Dim.angle $
                  Compound "/" [Constant (PrimNum 180), Constant (PrimVar "pi")])

meters = ("m", Unit Dim.length id id)
inches = ("in", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 10000 % 254))
feet = ("ft", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 10000 % 3048))
yards = ("yd", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 10000 % 9144))
miles = ("mi", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 1000 % 1609344))
astrounits = ("au", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 1 % 149597870700))
lightyears = ("lyr", unitByFactor Dim.length $ Constant (PrimNum . NRatio $ 1 % 9460730472580800))
parsecs = ("pc", unitByFactor Dim.length $ Compound "/" [Constant (PrimVar "pi"),
                                                         Constant (PrimNum 96939420213600000)])

seconds = ("s", Unit Dim.time id id)
seconds' = ("sec", snd seconds) -- Yes, this line reads "seconds prime is second second seconds" :)
minutes = ("min", unitByFactor Dim.time . recipOf $ Constant (PrimNum 60))
hours = ("hr", unitByFactor Dim.time . recipOf $ Constant (PrimNum 3600))
days = ("day", unitByFactor Dim.time . recipOf $ Constant (PrimNum 86400))
weeks = ("wk", unitByFactor Dim.time . recipOf $ Constant (PrimNum 604800))
years = ("yr", unitByFactor Dim.time . recipOf $ Constant (PrimNum 31557600))

liters = ("L", unitByFactor ((3 :: Int) `stimes` Dim.length) $ Constant (PrimNum . NRatio $ 1000 % 1))

table :: Map String (Unit (Expr Prim) (Expr Prim))
table = Map.fromList $ concat [

         -- Angular units
         [radians, degrees],

         -- Length units
         expandSIPrefixes meters,
         [inches, feet, yards, miles, astrounits, lightyears, parsecs],

        -- Time units
         expandSIPrefixes seconds,
         [seconds', minutes, hours, days, weeks, years],

         -- Volume units
         expandSIPrefixes liters

        ]

-- This is a cheap and dirty covert function. It's just for me to test
-- things right now.
--
-- TODO Clean it up and put it somewhere under Data.Calc.Function.*
simpleConvert :: Monad m => Function m
simpleConvert = function "__conv" go
    where go = do
            (expr, old, new) <- ask >>= parseArgs
            old' <- parseUnits table old
            new' <- parseUnits table new
            maybeToFail $ convert old' new' expr
          parseArgs [expr, old, new] = pure (expr, old, new)
          parseArgs [old, new] = pure (Constant (PrimNum 1), old, new)
          parseArgs _ = fail "invalid args to __conv"
