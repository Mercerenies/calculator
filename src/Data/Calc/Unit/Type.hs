{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module Data.Calc.Unit.Type(Unit(), RuntimeUnit(), UnitalValue(..), MatchingUnits(..),
                           unit, runtime,
                           compareDims,
                           valueUnit, valueValue, baseUnit,
                           ofDimension,
                           toBaseUnit, convertTo, convert) where

import Data.Calc.Unit.Dimension

import Data.Proxy
import Data.Coerce

data Unit (d :: Dimension) b a = Unit {
      unitName_ :: String,
      unitToBase_ :: a -> b,
      unitFromBase_ :: b -> a
    }

-- We ignore the dimension here so we can store it at runtime and then
-- provide a way to "convert" back.
data RuntimeUnit b a where
    RuntimeUnit :: Dimension -> Unit d b a -> RuntimeUnit b a

data MatchingUnits b a where
    MatchingUnits :: Unit d b a -> Unit d b a -> MatchingUnits b a

data UnitalValue (d :: Dimension) b a = UnitalValue (Unit d b a) a

instance Show (Unit d b a) where
    showsPrec _ x = (unitName_ x ++)

instance Show a => Show (UnitalValue d b a) where
    showsPrec n (UnitalValue u x) = showParen (n >= 7) $ showsPrec (max n 8) x . (" " ++) . shows u

unsafeTransmute :: Unit d b a -> Unit d' b a
unsafeTransmute = coerce

unit :: String -> (a -> b) -> (b -> a) -> Unit d b a
unit = Unit

runtime :: KnownDim d => Unit d b a -> RuntimeUnit b a
runtime u = RuntimeUnit (runDim $ pr u) u
    where pr :: Unit d' b' a' -> Proxy d'
          pr _ = Proxy

compareDims :: RuntimeUnit b a -> RuntimeUnit b a -> Maybe (MatchingUnits b a)
compareDims (RuntimeUnit d u) (RuntimeUnit d' u')
    | d == d' = Just (MatchingUnits (unsafeTransmute u) (unsafeTransmute u'))
    | otherwise = Nothing

valueUnit :: UnitalValue d b a -> Unit d b a
valueUnit (UnitalValue u _) = u

valueValue :: UnitalValue d b a -> a
valueValue (UnitalValue _ a) = a

baseUnit :: Unit d a a
baseUnit = unit "(base)" id id

ofDimension :: Unit d b a -> proxy d -> Unit d b a
ofDimension = const

toBaseUnit :: UnitalValue d b a -> UnitalValue d b b
toBaseUnit v = convertTo baseUnit v

convertTo :: Unit d b a' -> UnitalValue d b a -> UnitalValue d b a'
convertTo u' (UnitalValue u a) = UnitalValue u' . unitFromBase_ u' . unitToBase_ u $ a

convert :: Unit d b a -> Unit d b a' -> a -> a'
convert u u' a = valueValue $ convertTo u' (UnitalValue u a)
