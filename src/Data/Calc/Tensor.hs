{-# LANGUAGE ScopedTypeVariables #-}

module Data.Calc.Tensor(query, build, identity, prod, pow) where

import Data.Calc.Expr
import Data.Calc.Shape

import Data.Maybe
import Data.Array.ST
import Data.Array
import Data.STRef
import Control.Monad.ST

-- The functions here are definitely partial and do no bounds
-- checking. Use with caution.

data FastArray a = FastArray [Int] (Array Int a)
                   deriving (Eq)

faQuery :: [Int] -> FastArray a -> a
faQuery ns (FastArray dims arr) = arr ! n
    where n = sum . zipWith (*) ns . tail $ scanr (*) 1 dims

faBuild :: forall a. ([Int] -> a) -> [Int] -> FastArray a
faBuild f dims = FastArray dims $ runSTArray builder
    where totalSize = product dims
          builder :: ST s (STArray s Int a)
          builder = newArray (0, totalSize) undefined >>= \arr -> go 0 id dims arr >> return arr
          go acc acc1 [] arr = writeArray arr acc (f $ acc1 [])
          go acc acc1 (n:ns) arr = mapM_ (\i -> go (acc * n + i) (acc1 . (i :)) ns arr) [0..n-1]

faBuildExpr :: forall a. [Int] -> Expr a -> FastArray (Expr a)
faBuildExpr dims e = FastArray dims $ runSTArray builder
    where totalSize = product dims
          builder :: ST s (STArray s Int (Expr a))
          builder = do
            arr <- newArray (0, totalSize) undefined
            counter <- newSTRef (0 :: Int)
            go dims arr counter e
            return arr
          go :: [t0] -> STArray s Int (Expr a) -> STRef s Int -> Expr a -> ST s ()
          go [] arr counter e0 =
              readSTRef counter >>= \i -> writeArray arr i e0 >> modifySTRef counter (+ 1)
          go (_:ns) arr counter (Compound "vector" xs) = mapM_ (go ns arr counter) xs
          go _ _ _ _ = error "tensor faBuildExpr internal error"

faToExpr :: FastArray (Expr a) -> Expr a
faToExpr (FastArray dims arr) = go 0 dims
    where go a [] = arr ! a
          go a (n:ns) = Compound "vector" [go (a * n + i) ns | i <- [0..n-1]]

query :: [Int] -> Expr a -> Expr a
query [] x = x
query (n:ns) (Compound _ ts) = query ns (ts !! n)
query _ _ = error "tensor query out of bounds"

build :: ([Int] -> Expr a) -> [Int] -> Expr a
build f dims = go id dims
    where go acc [] = f (acc [])
          go acc (n:ns) = Compound "vector" [go (acc . (i :)) ns | i <- [0..n-1]]

-- Identity matrix
faIdentity :: Int -> FastArray (Expr Prim)
faIdentity n = faBuild f [n, n]
    where f [i, j] | i == j    = Constant (PrimNum 1)
                   | otherwise = Constant (PrimNum 0)
          f _ = error "internal error in identity matrix"

-- Identity matrix
identity :: Int -> Expr Prim
identity n = build f [n, n]
    where f [i, j] | i == j    = Constant (PrimNum 1)
                   | otherwise = Constant (PrimNum 0)
          f _ = error "internal error in identity matrix"

-- I already know this is pathetically slow because of all the random
-- access. Either make the general algorithm much faster or specialize
-- for the common case of matrices so we can take fast matrix powers.
_legacyProd :: Expr a -> Expr a -> Maybe (Expr a)
_legacyProd x y
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

faProd :: FastArray (Expr a) -> FastArray (Expr a) -> Maybe (FastArray (Expr a))
faProd (x @ (FastArray xd _)) (y @ (FastArray yd _))
    | not (null xd) && not (null yd)
    , last xd == head yd
    , let mergedim = length xd - 1
    , let newdims = init xd ++ tail yd
    , let sum' = Compound "+"
    , let a .* b = Compound "*" [a, b]
    , let f ns = sum' [faQuery (take (length xd - 1) ns ++ [i]) x .*
                       faQuery ([i] ++ drop (length xd - 1) ns) y
                             | i <- [0 .. (xd !! mergedim) - 1]]
    = Just $ faBuild f newdims
    | otherwise
    = Nothing

prod :: Expr a -> Expr a -> Maybe (Expr a)
prod x y
    | Just xd <- vectorDims x
    , Just yd <- vectorDims y
    = faToExpr <$> faProd (faBuildExpr xd x) (faBuildExpr yd y)
    | otherwise
    = Nothing

faPow :: FastArray (Expr Prim) -> Integer -> Maybe (FastArray (Expr Prim))
faPow (x @ (FastArray dims _)) n
    | [a, b] <- dims
    , n >= 0 -- TODO Negative integers
    , a == b
    , let go _ 0 = faIdentity a
          go y 1 = y
          go y 2 = fromJust (y `faProd` y) -- I know the dims line up, so fromJust is safe
          go y m
              | even m    = go (go y (m `div` 2)) 2
              | otherwise = fromJust (go y (m - 1) `faProd` y)
    = Just (go x n)
    | otherwise
    = Nothing

-- Right now, this only works on square matrices and not on any other
-- tensors. These are the only ones we can guarantee the behavior for
-- for abstract positive integer powers.
_legacyPow :: Expr Prim -> Integer -> Maybe (Expr Prim)
_legacyPow x n
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

pow :: Expr Prim -> Integer -> Maybe (Expr Prim)
pow x n
    | Just xd <- vectorDims x = faToExpr <$> faPow (faBuildExpr xd x) n
    | otherwise = Nothing
