
module Data.Calc.Unit.Type(Unit(..), UnitalValue(..),
                           baseUnit, toBaseUnit,
                           unsafeConvertTo, unsafeConvert,
                           convertTo, convert, (.*), (./), recip, (.^)) where

import Data.Calc.Unit.Dimension(Dimension)
import qualified Data.Calc.Unit.Dimension as Dim

import Prelude hiding (id, (.))
import Control.Category

data Unit b a = Unit {
      unitDim :: Dimension,
      unitToBase :: a -> b,
      unitFromBase :: b -> a
    }

data UnitalValue b a = UnitalValue {
      valueUnit :: Unit b a,
      valueValue :: a
    }

instance Category Unit where
    id = baseUnit Dim.unitless
    (.) = flip (.*)

baseUnit :: Dimension -> Unit a a
baseUnit d = Unit d id id

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

(.*) :: Unit c b -> Unit b a -> Unit c a
Unit d t f .* Unit d' t' f' = Unit (Dim.mul d d') (t . t') (f' . f)

invUnit :: Unit b a -> Unit a b
invUnit (Unit d t f) = Unit (Dim.recip d) f t

(./) :: Unit c b -> Unit a b -> Unit c a
u ./ u' = u .* invUnit u'

(.^) :: Unit a a -> Int -> Unit a a
u .^ n
    | n < 0 = invUnit u .^ (- n)
    | otherwise = foldr (.) id $ replicate n u

