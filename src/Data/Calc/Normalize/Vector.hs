{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Normalize.Vector(vectorAddition, scalarMultiplication,
                                  matrixMultiplication, vectorPass) where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode
import Data.Calc.Shape
import Data.Calc.Function.Type
import qualified Data.Calc.Tensor as Tensor

import Prelude hiding (fail, (.), id)
import Control.Monad.Reader
import Control.Applicative
import Data.Map(Map)
import qualified Data.List.NonEmpty as NonEmpty

vectorAddition :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
vectorAddition fns = PassT go
    where go (Compound "+" xs) = Compound "+" <$> (ask >>= \m -> pure $ attempt m xs)
          go x = pure x
          attempt m (x:y:zs)
              -- Vector-vector addition
              | Compound "vector" xs <- x
              , Compound "vector" ys <- y
              , Just xd <- vectorDims x
              , Just yd <- vectorDims y
              , xd == yd
              , let zipped = zipWith (\a b -> Compound "+" [a, b]) xs ys
              = attempt m $ Compound "vector" zipped : zs
              -- Vector-scalar addition
              | Compound "vector" xs <- x
              , makeAssumptions (shapeOf fns y) m == Scalar
              , let zipped = map (\a -> Compound "+" [a, y]) xs
              = attempt m $ Compound "vector" zipped : zs
              -- Scalar-vector addition
              | Compound "vector" ys <- y
              , makeAssumptions (shapeOf fns x) m == Scalar
              , let zipped = map (\b -> Compound "+" [x, b]) ys
              = attempt m $ Compound "vector" zipped : zs
          attempt m (x:xs) = x : attempt m xs
          attempt _ [] = []

scalarMultiplication :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
scalarMultiplication fns = PassT go
    where go (Compound "*" xs) = Compound "*" <$> attempt xs
          go x = pure x
          attempt [] = pure []
          attempt [x] = pure [x]
          attempt (x:y:zs) =
              liftA2 mplus (tryMultiply x y) (tryMultiply y x) >>= \case
                Nothing -> (x :) <$> attempt (y : zs)
                Just r -> attempt (r : zs)
          tryMultiply s (Compound "vector" xs) =
              makeAssumptions (shapeOf fns s) >>= \case
                Scalar -> pure . Just . Compound "vector" $ map (\x -> Compound "*" [s, x]) xs
                _ -> pure Nothing
          tryMultiply _ _ = pure Nothing

matrixMultiplication :: Monad m => PassT m Prim Prim
matrixMultiplication = pass go
    where go (Compound "*" xs) = Compound "*" $ attempt xs
          go x = x
          attempt [] = []
          attempt [x] = [x]
          attempt (x:y:zs)
              | Just xd  <- vectorDims x
              , Just yd  <- vectorDims y
              , Just xd' <- NonEmpty.nonEmpty xd
              , Just yd' <- NonEmpty.nonEmpty yd
              , NonEmpty.last xd' == NonEmpty.head yd'
              , let mergedim = length xd - 1
              , let newdims = NonEmpty.init xd' ++ NonEmpty.tail yd'
              , let sum' = Compound "+"
              , let a .* b = Compound "*" [a, b]
              , let f ns =
                        sum' [Tensor.query (take (length xd - 1) ns ++ [i]) x .*
                              Tensor.query ([i] ++ drop (length xd - 1) ns) y
                                  | i <- [0 .. (xd !! mergedim) - 1]]
              = attempt (Tensor.build f newdims : zs)
              | otherwise = x : attempt (y:zs)

vectorPass :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
vectorPass fns = matrixMultiplication . scalarMultiplication fns . vectorAddition fns
