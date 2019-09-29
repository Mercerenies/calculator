
module Data.Calc.Tensor where

import Data.Calc.Expr

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
