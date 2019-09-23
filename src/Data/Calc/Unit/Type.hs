
module Data.Calc.Unit.Type(Unit(), UnitalValue(..),
                           unit, baseUnit,
                           toBaseUnit,
                           unsafeConvertTo, unsafeConvert,
                           convertTo, convert) where

import Data.Calc.Unit.Dimension

data Unit b a = Unit {
      unitName_ :: String,
      unitDim_ :: Dimension,
      unitToBase_ :: a -> b,
      unitFromBase_ :: b -> a
    }

data UnitalValue b a = UnitalValue {
      valueUnit :: Unit b a,
      valueValue :: a
    }

instance Show (Unit b a) where
    showsPrec _ x = (unitName_ x ++)

instance Show a => Show (UnitalValue b a) where
    showsPrec n (UnitalValue u x) = showParen (n >= 7) $ showsPrec (max n 8) x . (" " ++) . shows u

unit :: String -> Dimension -> (a -> b) -> (b -> a) -> Unit b a
unit = Unit

baseUnit :: Dimension -> Unit a a
baseUnit d = unit "(base)" d id id

toBaseUnit :: UnitalValue b a -> UnitalValue b b
toBaseUnit v = let d = unitDim_ $ valueUnit v
               in unsafeConvertTo (baseUnit d) v

unsafeConvertTo :: Unit b a' -> UnitalValue b a -> UnitalValue b a'
unsafeConvertTo u' (UnitalValue u a) = UnitalValue u' . unitFromBase_ u' . unitToBase_ u $ a

convertTo :: Unit b a' -> UnitalValue b a -> Maybe (UnitalValue b a')
convertTo u' (UnitalValue u a)
    | unitDim_ u == unitDim_ u' = Just $ unsafeConvertTo u' (UnitalValue u a)
    | otherwise = Nothing

unsafeConvert :: Unit b a -> Unit b a' -> a -> a'
unsafeConvert u u' a = valueValue $ unsafeConvertTo u' (UnitalValue u a)

convert :: Unit b a -> Unit b a' -> a -> Maybe a'
convert u u' a = valueValue <$> convertTo u' (UnitalValue u a)
