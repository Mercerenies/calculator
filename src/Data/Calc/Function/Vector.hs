{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Vector(fdet) where

import Data.Calc.Function.Type
import Data.Calc.Function.Shape
import Data.Calc.Shape
import Data.Calc.Util
import Data.Calc.Expr

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Reader hiding (fail)

matrixToList :: Expr Prim -> Maybe [[Expr Prim]]
matrixToList (Compound "vector" xs) = mapM go xs
    where go (Compound "vector" ys) = Just ys
          go _ = Nothing
matrixToList _ = Nothing

det :: [[Expr Prim]] -> Expr Prim
det = go
    where a .* b = Compound "*" [a, b]
          go []  = Constant (PrimNum 1)
          go xss = Compound "+" . altsum $
                   zipWith (.*) (map head xss) (map go . minors $ map tail xss)
          minors [] = []
          minors (xs:xss) = (xss :) . map (xs :) $ minors xss
          altsum [] = []
          altsum [x] = [x]
          altsum (x:y:zs) = [x, Constant (PrimNum $ -1) .* y] ++ zs

fdet :: Monad m => Function m
fdet = function "det" f `withShape` always Scalar
    where f = do
            [x] <- ask
            [a, b] <- maybeToFail $ vectorDims x
            guard $ a == b
            x' <- maybeToFail $ matrixToList x
            return $ det x'
