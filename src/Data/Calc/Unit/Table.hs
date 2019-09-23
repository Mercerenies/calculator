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

unitByFactor :: String -> Expr Prim -> Unit d (Expr Prim) (Expr Prim)
unitByFactor name factor =
    unit name (multiplyBy $ Compound "/" [Constant $ PrimNum 1, factor]) (multiplyBy factor)

radians :: Unit 'Angle (Expr Prim) (Expr Prim)
radians = unit "rad" id id

degrees :: Unit 'Angle (Expr Prim) (Expr Prim)
degrees = unitByFactor "deg" $ Compound "/" [Constant (PrimNum 180), Constant (PrimVar "pi")]

meters :: Unit 'Length (Expr Prim) (Expr Prim)
meters = unit "m" id id

centimeters :: Unit 'Length (Expr Prim) (Expr Prim)
centimeters = unitByFactor "cm" $ Constant (PrimNum 100)

table :: Map String (RuntimeUnit (Expr Prim) (Expr Prim))
table = Map.fromList [
         ("rad", runtime radians),
         ("deg", runtime degrees),
         ("m", runtime meters),
         ("cm", runtime centimeters)
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
            MatchingUnits old'' new'' <- maybeToFail $ compareDims old' new'
            return $ convert old'' new'' expr
