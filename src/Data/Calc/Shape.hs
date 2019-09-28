{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Shape where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Util

import Control.Monad.Reader

data Shape = Scalar | Vector | Matrix | Variable | Unknown
             deriving (Show, Read, Eq, Ord, Enum)

vectorDims :: Expr Prim -> Maybe [Int]
vectorDims (Compound "vector" xs) = (length xs :) <$> (mapM vectorDims xs >>= the)
vectorDims _ = Just []

shapeOf :: Expr Prim -> Shape
shapeOf (Constant (PrimNum _)) = Scalar
shapeOf (Constant (PrimVar _)) = Variable
shapeOf (Compound "vector" xs) = case vectorDims (Compound "vector" xs) of
                                   Nothing -> Vector
                                   Just x | length x > 1 -> Matrix
                                          | otherwise    -> Vector
shapeOf (Compound _ _) = Unknown -- TODO We know what some functions
                                 -- return; encode it, esp for +, *,
                                 -- etc.

makeAssumptions :: MonadReader ModeInfo m => Shape -> m Shape
makeAssumptions Variable = go <$> asks vectorMode
    where go AssumeNothing = Variable
          go AssumeMatrix  = Matrix
          go AssumeScalar  = Scalar
makeAssumptions x = pure x
