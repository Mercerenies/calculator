
module Data.Calc.Tensor where

import Data.Calc.Expr
import Data.Calc.Shape

-- The functions here are definitely partial and do no bounds
-- checking. Use with caution.

query :: [Int] -> Expr a -> Expr a
query [] x = x
query (n:ns) (Compound _ ts) = query ns (ts !! n)
query _ _ = error "tensor query out of bounds"

build :: ([Int] -> Expr a) -> [Int] -> Expr a
build f dims = go id dims
    where go acc [] = f (acc [])
          go acc (n:ns) = Compound "vector" [go (acc . (i :)) ns | i <- [0..n-1]]

prod :: Expr a -> Expr a -> Maybe (Expr a)
prod x y
    | Just xd <- vectorDims x
    , Just yd <- vectorDims y
    , not (null xd) && not (null yd)
    , last xd == head yd
    , let mergedim = length xd - 1
    , let newdims = init xd ++ tail yd
    , let sum' = Compound "+"
    , let a .* b = Compound "*" [a, b]
    , let f ns = sum' [query (take (length xd - 1) ns ++ [i]) x .*
                       query ([i] ++ drop (length xd - 1) ns) x
                           | i <- [0 .. (xd !! mergedim) - 1]]
    = Just $ build f newdims
    | otherwise
    = Nothing
