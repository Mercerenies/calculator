
module Data.Calc.Function where

import Data.Calc.Expr

import Data.Map(Map)
import qualified Data.Map as Map

fsin :: [Expr Prim] -> Maybe (Expr Prim)
fsin [Constant (PrimNum a)] = Just $ Constant (PrimNum $ sin a)
fsin _ = Nothing

stdBuiltins :: Map String ([Expr Prim] -> Maybe (Expr Prim))
stdBuiltins = Map.fromList [
            ("sin", fsin)
           ]

applyTo :: Map String ([Expr Prim] -> Maybe (Expr Prim)) -> String -> [Expr Prim] -> Expr Prim
applyTo m s args = case Map.lookup s m >>= ($ args) of
                     Nothing -> Compound s args
                     Just x  -> x

applyToStd :: String -> [Expr Prim] -> Expr Prim
applyToStd = applyTo stdBuiltins
