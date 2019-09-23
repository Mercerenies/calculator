
module Data.Calc.Unit.Type(Unit(..), UnitalValue(..),
                           baseUnit,
                           toBaseUnit,
                           unsafeConvertTo, unsafeConvert,
                           convertTo, convert) where

import Data.Calc.Unit.Dimension

data Unit b a = Unit {
      unitName :: String,
      unitDim :: Dimension,
      unitToBase :: a -> b,
      unitFromBase :: b -> a
    }

data UnitalValue b a = UnitalValue {
      valueUnit :: Unit b a,
      valueValue :: a
    }

instance Show (Unit b a) where
    showsPrec _ x = (unitName x ++)

instance Show a => Show (UnitalValue b a) where
    showsPrec n (UnitalValue u x) = showParen (n >= 7) $ showsPrec (max n 8) x . (" " ++) . shows u

baseUnit :: Dimension -> Unit a a
baseUnit d = Unit "(base)" d id id

toBaseUnit :: UnitalValue b a -> UnitalValue b b
toBaseUnit v = let d = unitDim $ valueUnit v
               in unsafeConvertTo (baseUnit d) v

unsafeConvertTo :: Unit b a' -> UnitalValue b a -> UnitalValue b a'
unsafeConvertTo u' (UnitalValue u a) = UnitalValue u' . unitFromBase u' . unitToBase u $ a

convertTo :: Unit b a' -> UnitalValue b a -> Maybe (UnitalValue b a')
convertTo u' (UnitalValue u a)
    | unitDim u == unitDim u' = Just $ unsafeConvertTo u' (UnitalValue u a)
    | otherwise = Nothing

unsafeConvert :: Unit b a -> Unit b a' -> a -> a'
unsafeConvert u u' a = valueValue $ unsafeConvertTo u' (UnitalValue u a)

convert :: Unit b a -> Unit b a' -> a -> Maybe a'
convert u u' a = valueValue <$> convertTo u' (UnitalValue u a)
