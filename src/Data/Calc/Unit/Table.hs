{-# LANGUAGE DataKinds #-}

module Data.Calc.Unit.Table where

import Data.Calc.Unit.Type
import Data.Calc.Unit.Dimension
import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Util
--import Data.Calc.Number

import Data.Map(Map)
import qualified Data.Map as Map
--import Data.Ratio
import Control.Monad.Reader

multiplyBy :: Expr a -> Expr a -> Expr a
multiplyBy x y = Compound "*" [x, y]

unitByFactor :: String -> Dimension -> Expr Prim -> Unit (Expr Prim) (Expr Prim)
unitByFactor name dim factor =
    unit name dim (multiplyBy $ Compound "/" [Constant $ PrimNum 1, factor]) (multiplyBy factor)

radians :: Unit (Expr Prim) (Expr Prim)
radians = unit "rad" Angle id id

degrees :: Unit (Expr Prim) (Expr Prim)
degrees = unitByFactor "deg" Angle $ Compound "/" [Constant (PrimNum 180), Constant (PrimVar "pi")]

meters :: Unit (Expr Prim) (Expr Prim)
meters = unit "m" Length id id

centimeters :: Unit (Expr Prim) (Expr Prim)
centimeters = unitByFactor "cm" Length $ Constant (PrimNum 100)

table :: Map String (Unit (Expr Prim) (Expr Prim))
table = Map.fromList [
         ("rad", radians),
         ("deg", degrees),
         ("m", meters),
         ("cm", centimeters)
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
