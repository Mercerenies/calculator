{-# LANGUAGE RecordWildCards #-}

module Data.Calc.Unit.Table where

import Data.Calc.Unit.Type
import Data.Calc.Unit.Dimension
import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Util

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Arrow

multiplyBy :: Expr a -> Expr a -> Expr a
multiplyBy x y = Compound "*" [x, y]

recipOf :: Expr Prim -> Expr Prim
recipOf n = Compound "/" [Constant $ PrimNum 1, n]

unitByFactor :: String -> Dimension -> Expr Prim -> Unit (Expr Prim) (Expr Prim)
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

radians :: Unit (Expr Prim) (Expr Prim)
radians = Unit "rad" Angle id id

degrees :: Unit (Expr Prim) (Expr Prim)
degrees = unitByFactor "deg" Angle $ Compound "/" [Constant (PrimNum 180), Constant (PrimVar "pi")]

meters :: Unit (Expr Prim) (Expr Prim)
meters = Unit "m" Length id id

seconds :: Unit (Expr Prim) (Expr Prim)
seconds = Unit "s" Time id id

seconds' :: Unit (Expr Prim) (Expr Prim)
seconds' = synonym "sec" seconds

minutes :: Unit (Expr Prim) (Expr Prim)
minutes = unitByFactor "min" Time . recipOf $ Constant (PrimNum 60)

compileUnits :: [Unit b a] -> Map String (Unit b a)
compileUnits = map (unitName &&& id) >>> Map.fromList

table :: Map String (Unit (Expr Prim) (Expr Prim))
table = compileUnits $ concat [

         -- Angular units
         [radians, degrees],

         -- Length units
         expandSIPrefixes meters,

        -- Time units
         expandSIPrefixes seconds,
         [seconds', minutes]

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
