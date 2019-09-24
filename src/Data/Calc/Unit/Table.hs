{-# LANGUAGE RecordWildCards #-}

module Data.Calc.Unit.Table(expandSIPrefixes, radians, degrees, table, simpleConvert) where

import Data.Calc.Unit.Type
import qualified Data.Calc.Unit.Dimension as Dim
import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Util
import Data.Calc.Number

import Data.Ratio
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Arrow

-- A lot of the measurements in this file are from the Emacs Calc
-- units table.

multiplyBy :: Expr a -> Expr a -> Expr a
multiplyBy x y = Compound "*" [x, y]

recipOf :: Expr Prim -> Expr Prim
recipOf n = Compound "/" [Constant $ PrimNum 1, n]

unitByFactor :: String -> Dim.Dimension -> Expr Prim -> Unit (Expr Prim) (Expr Prim)
unitByFactor name dim factor =
    Unit name dim (multiplyBy $ recipOf factor) (multiplyBy factor)

expandSIPrefixes :: Unit (Expr Prim) (Expr Prim) -> [Unit (Expr Prim) (Expr Prim)]
expandSIPrefixes (Unit {..}) = fmap go prefixes
    where go (prefix, n) =
              let n' = Constant . PrimNum $ 10 ^ n
              in Unit (prefix ++ unitName) unitDim (multiplyBy n' . unitToBase)
                                                   (unitFromBase . multiplyBy (recipOf n'))
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
 seconds, seconds', minutes, hours, days, weeks, years
    :: Unit (Expr Prim) (Expr Prim)

radians = Unit "rad" Dim.angle id id
degrees = unitByFactor "deg" Dim.angle $
          Compound "/" [Constant (PrimNum 180), Constant (PrimVar "pi")]

meters = Unit "m" Dim.length id id
inches = unitByFactor "in" Dim.length $ Constant (PrimNum . NRatio $ 10000 % 254)
feet = unitByFactor "ft" Dim.length $ Constant (PrimNum . NRatio $ 10000 % 3048)
yards = unitByFactor "yd" Dim.length $ Constant (PrimNum . NRatio $ 10000 % 9144)
miles = unitByFactor "mi" Dim.length $ Constant (PrimNum . NRatio $ 1000 % 1609344)
astrounits = unitByFactor "au" Dim.length $ Constant (PrimNum . NRatio $ 1 % 149597870700)
lightyears = unitByFactor "lyr" Dim.length $ Constant (PrimNum . NRatio $ 1 % 9460730472580800)
parsecs = unitByFactor "pc" Dim.length $ Compound "/" [Constant (PrimVar "pi"),
                                                   Constant (PrimNum 96939420213600000)]

seconds = Unit "s" Dim.time id id
seconds' = synonym "sec" seconds
minutes = unitByFactor "min" Dim.time . recipOf $ Constant (PrimNum 60)
hours = unitByFactor "hr" Dim.time . recipOf $ Constant (PrimNum 3600)
days = unitByFactor "day" Dim.time . recipOf $ Constant (PrimNum 86400)
weeks = unitByFactor "wk" Dim.time . recipOf $ Constant (PrimNum 604800)
years = unitByFactor "yr" Dim.time . recipOf $ Constant (PrimNum 31557600)

compileUnits :: [Unit b a] -> Map String (Unit b a)
compileUnits = map (unitName &&& id) >>> Map.fromList

table :: Map String (Unit (Expr Prim) (Expr Prim))
table = compileUnits $ concat [

         -- Angular units
         [radians, degrees],

         -- Length units
         expandSIPrefixes meters,
         [inches, feet, yards, miles, astrounits, lightyears, parsecs],

        -- Time units
         expandSIPrefixes seconds,
         [seconds', minutes, hours, days, weeks, years]

        ]

-- This is a cheap and dirty covert function. It's just for me to test
-- things right now.
--
-- TODO Clean it up and put it somewhere under Data.Calc.Function.*
simpleConvert :: Monad m => Function m
simpleConvert = function "__conv" go
    where go = do
            [expr, Constant (PrimVar old), Constant (PrimVar new)] <- ask
            old' <- maybeToFail $ Map.lookup old table
            new' <- maybeToFail $ Map.lookup new table
            maybeToFail $ convert old' new' expr
