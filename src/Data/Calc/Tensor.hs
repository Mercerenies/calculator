
module Data.Calc.Tensor where

import Data.Calc.Expr
import Data.Calc.Shape

import Data.Maybe

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

-- Identity matrix
identity :: Int -> Expr Prim
identity n = build f [n, n]
    where f [i, j] | i == j    = Constant (PrimNum 1)
                   | otherwise = Constant (PrimNum 0)
          f _ = error "internal error in identity matrix"

-- TODO I already know this is pathetically slow because of all the
-- random access. Either make the general algorithm much faster or
-- specialize for the common case of matrices so we can take fast matrix powers.
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
                       query ([i] ++ drop (length xd - 1) ns) y
                           | i <- [0 .. (xd !! mergedim) - 1]]
    = Just $ build f newdims
    | otherwise
    = Nothing

-- Right now, this only works on square matrices and not on any other
-- tensors. These are the only ones we can guarantee the behavior for
-- for abstract positive integer powers.
pow :: Expr Prim -> Integer -> Maybe (Expr Prim)
pow x n
    | Just [a, b] <- vectorDims x
    , n >= 0 -- TODO Negative integers
    , a == b
    , let go _ 0 = identity a
          go y 1 = y
          go y 2 = fromJust (y `prod` y) -- I know the dims line up, so fromJust is safe
          go y m
              | even m    = go (go y (m `div` 2)) 2
              | otherwise = fromJust (go y (m - 1) `prod` y)
    = Just (go x n)
    | otherwise
    = Nothing
