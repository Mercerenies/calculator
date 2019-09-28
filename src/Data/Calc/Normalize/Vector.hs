{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Calc.Normalize.Vector where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode
import Data.Calc.Shape

import Prelude hiding (fail, (.), id)
import Control.Monad.Reader
import Control.Applicative

vectorAddition :: Monad m => PassT m Prim Prim
vectorAddition = pass go
    where go (Compound "+" xs) = Compound "+" $ attempt xs
          go x = x
          attempt (x:y:zs)
              | Compound "vector" xs <- x
              , Compound "vector" ys <- y
              , Just xd <- vectorDims x
              , Just yd <- vectorDims y
              , xd == yd
              , let zipped = zipWith (\a b -> Compound "+" [a, b]) xs ys
              = attempt $ Compound "vector" zipped : zs
          attempt (x:xs) = x : attempt xs
          attempt [] = []

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
