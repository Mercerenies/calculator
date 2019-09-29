{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Normalize.Vector(vectorAddition, scalarMultiplication,
                                  matrixMultiplication, vectorPass) where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode
import Data.Calc.Shape
import Data.Calc.Util
import Data.Calc.Function.Type
import qualified Data.Calc.Tensor as Tensor

import Prelude hiding (fail, (.), id)
import Control.Monad.Reader
import Control.Applicative
import Data.Map(Map)

vectorAddition :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
vectorAddition fns = PassT go
    where go (Compound "+" xs) =
              Compound "+" <$> (ask >>= \m -> pure $ pairwiseAttempt (attempt m) xs)
          go x = pure x
          attempt m x y
              -- Vector-vector addition
              | Compound "vector" xs <- x
              , Compound "vector" ys <- y
              , Just xd <- vectorDims x
              , Just yd <- vectorDims y
              , xd == yd
              , let zipped = zipWith (\a b -> Compound "+" [a, b]) xs ys
              = Just (Compound "vector" zipped)
              -- Vector-scalar addition
              | Compound "vector" xs <- x
              , makeAssumptions (shapeOf fns y) m == Scalar
              , let zipped = map (\a -> Compound "+" [a, y]) xs
              = Just (Compound "vector" zipped)
              -- Scalar-vector addition
              | Compound "vector" ys <- y
              , makeAssumptions (shapeOf fns x) m == Scalar
              , let zipped = map (\b -> Compound "+" [x, b]) ys
              = Just (Compound "vector" zipped)
              | otherwise
              = Nothing

scalarMultiplication :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
scalarMultiplication fns = PassT go
    where go (Compound "*" xs) = Compound "*" <$> pairwiseAttemptM attempt xs
          go x = pure x
          attempt x y = liftA2 mplus (tryMultiply x y) (tryMultiply y x)
          tryMultiply s (Compound "vector" xs) =
              makeAssumptions (shapeOf fns s) >>= \case
                Scalar -> pure . Just . Compound "vector" $ map (\x -> Compound "*" [s, x]) xs
                _ -> pure Nothing
          tryMultiply _ _ = pure Nothing

matrixMultiplication :: Monad m => PassT m Prim Prim
matrixMultiplication = pass go
    where go (Compound "*" xs) = Compound "*" $ (pairwiseAttempt attempt) xs
          go x = x
          attempt x y = Tensor.prod x y

vectorPass :: MonadReader ModeInfo m => Map String (Function m) -> PassT m Prim Prim
vectorPass fns = matrixMultiplication . scalarMultiplication fns . vectorAddition fns
