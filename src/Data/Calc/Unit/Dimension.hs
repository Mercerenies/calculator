{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Calc.Unit.Dimension(SimpleDim(..), Dimension(..),
                                singleton, unitless, (.*), recip, (./),
                                angle, length, time) where

import Data.Map(Map)
import qualified Data.Map as Map
import Prelude hiding (recip, length)

data SimpleDim = Angle | Length | Time
                 deriving (Show, Read, Eq, Ord, Enum)

newtype Dimension = Dimension (Map SimpleDim Int)
    deriving (Show, Read, Eq)

singleton :: SimpleDim -> Dimension
singleton dim = Dimension $ Map.singleton dim 1

unitless :: Dimension
unitless = Dimension Map.empty

(.*) :: Dimension -> Dimension -> Dimension
Dimension a .* Dimension b = Dimension $ Map.unionWith (+) a b

recip :: Dimension -> Dimension
recip (Dimension a) = Dimension (fmap negate a)

(./) :: Dimension -> Dimension -> Dimension
a ./ b = a .* recip b

angle, length, time :: Dimension
angle = singleton Angle
length = singleton Length
time = singleton Time
