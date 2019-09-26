{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Calc.Unit.Dimension(SimpleDim(..), Dimension(),
                                singleton, unitless, mul, recip, div,
                                angle, length, time) where

import Data.Semigroup
import Data.Map(Map)
import qualified Data.Map as Map
import Prelude hiding (recip, length, div)

data SimpleDim = Angle | Length | Time
                 deriving (Show, Read, Eq, Ord, Enum)

newtype Dimension = Dimension (Map SimpleDim Int)
    deriving (Show, Read, Eq)

instance Semigroup Dimension where
    (<>) = mul

instance Monoid Dimension where
    mempty = unitless
    mappend = (<>)

normalize :: Dimension -> Dimension
normalize (Dimension m) = Dimension $ Map.filter (/= 0) m

singleton :: SimpleDim -> Dimension
singleton dim = Dimension $ Map.singleton dim 1

unitless :: Dimension
unitless = Dimension Map.empty

mul :: Dimension -> Dimension -> Dimension
Dimension a `mul` Dimension b = normalize . Dimension $ Map.unionWith (+) a b

recip :: Dimension -> Dimension
recip (Dimension a) = normalize $ Dimension (fmap negate a)

div :: Dimension -> Dimension -> Dimension
a `div` b = a `mul` recip b

angle, length, time :: Dimension
angle = singleton Angle
length = singleton Length
time = singleton Time
