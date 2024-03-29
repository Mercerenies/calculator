{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Shape(Shape(..), vectorDims, shapeOf, shapeOf',
                       makeAssumptions, multiplicationCommutes) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Util
import Data.Calc.Function.Type
import Data.Calc.Shape.Type

import Control.Monad.Reader
import Data.Foldable
import Data.Map(Map)
import qualified Data.Map as Map

shapeSum :: Shape -> Shape -> Shape
shapeSum Unknown _ = Unknown
shapeSum _ Unknown = Unknown
shapeSum Scalar x = x
shapeSum x Scalar = x
shapeSum Variable x = x
shapeSum x Variable = x
shapeSum Vector Vector = Vector
shapeSum Vector Matrix = Unknown
shapeSum Matrix Matrix = Matrix
shapeSum Matrix Vector = Unknown

shapeMul :: Shape -> Shape -> Shape
shapeMul Unknown _ = Unknown
shapeMul _ Unknown = Unknown
shapeMul Scalar x = x
shapeMul x Scalar = x
shapeMul Variable x = x
shapeMul x Variable = x
shapeMul Vector Vector = Scalar
shapeMul Vector Matrix = Vector
shapeMul Matrix Matrix = Matrix
shapeMul Matrix Vector = Vector
-- TODO Matrix-Vector and Vector-Matrix won't actually run right now,
-- but we could write a pass for them easily as the operations are
-- useful and well-defined.

vectorDims :: Expr a -> Maybe [Int]
vectorDims (Compound "vector" xs) = (length xs :) <$> (mapM vectorDims xs >>= the)
vectorDims _ = Just []

shapeOf :: Map String (Function m) -> Expr Prim -> Shape
shapeOf _ (Constant (PrimNum _)) = Scalar
shapeOf _ (Constant (PrimVar _)) = Variable
shapeOf m (Compound h ts) =
    case h of
      "vector" -> case vectorDims (Compound "vector" ts) of
                    Nothing -> Vector
                    Just x | length x > 1 -> Matrix
                           | otherwise    -> Vector
      "+" -> foldl' shapeSum Scalar (map (shapeOf m) ts)
      "*" -> foldl' shapeMul Scalar (map (shapeOf m) ts)
      "/" -> foldl' shapeMul Scalar (map (shapeOf m) ts) -- Same rules as *
      "^" -> case ts of
               [] -> Unknown -- What...?
               (y:_) -> shapeOf m y
      h' | Just f <- Map.lookup h' m -> fnShape f (shapeOf m) ts
      _ -> Unknown

-- Limited dumbed-down version of shapeOf that doesn't require the map
shapeOf' :: Expr Prim -> Shape
shapeOf' = shapeOf Map.empty

makeAssumptions :: MonadReader ModeInfo m => Shape -> m Shape
makeAssumptions Variable = go <$> asks vectorMode
    where go AssumeNothing = Variable
          go AssumeMatrix  = Matrix
          go AssumeScalar  = Scalar
makeAssumptions x = pure x

multiplicationCommutes :: Shape -> Bool
multiplicationCommutes shape =
    case shape of
      Scalar -> True
      Vector -> False -- Can't commute with matrices but can commute with other vectors, so play safe.
      Matrix -> False
      Variable -> True -- If it still shows as a variable after assumptions, say we're okay.
      Unknown -> False
