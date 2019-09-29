{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Normalize.Vector where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode
import Data.Calc.Shape

import Prelude hiding (fail, (.), id)
import Control.Monad.Reader
import Control.Applicative

vectorAddition :: MonadReader ModeInfo m => PassT m Prim Prim
vectorAddition = PassT go
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
              , makeAssumptions (shapeOf y) m == Scalar
              , let zipped = map (\a -> Compound "+" [a, y]) xs
              = attempt m $ Compound "vector" zipped : zs
              -- Scalar-vector addition
              | Compound "vector" ys <- y
              , makeAssumptions (shapeOf x) m == Scalar
              , let zipped = map (\b -> Compound "+" [x, b]) ys
              = attempt m $ Compound "vector" zipped : zs
          attempt m (x:xs) = x : attempt m xs
          attempt _ [] = []

scalarMultiplication :: MonadReader ModeInfo m => PassT m Prim Prim
scalarMultiplication = PassT go
    where go (Compound "*" xs) = Compound "*" <$> attempt xs
          go x = pure x
          attempt [] = pure []
          attempt [x] = pure [x]
          attempt (x:y:zs) =
              liftA2 mplus (tryMultiply x y) (tryMultiply y x) >>= \case
                Nothing -> (x :) <$> attempt (y : zs)
                Just r -> attempt (r : zs)
          tryMultiply s (Compound "vector" xs) =
              makeAssumptions (shapeOf s) >>= \case
                Scalar -> pure . Just . Compound "vector" $ map (\x -> Compound "*" [s, x]) xs
                _ -> pure Nothing
          tryMultiply _ _ = pure Nothing

vectorPass :: MonadReader ModeInfo m => PassT m Prim Prim
vectorPass = scalarMultiplication . vectorAddition
