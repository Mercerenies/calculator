{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Calc.Number(Number(..), RealNum(..), toDouble, Complex(..)) where

import Data.Ratio
import Data.Complex
import Control.Arrow

newtype Number = Number (Complex RealNum)
    deriving (Eq, Num, Fractional)

data RealNum = NRatio Rational
             | NDouble Double

instance Show Number where
    showsPrec n (Number (a :+ b))
        | b == 0 = showsPrec n a
        | otherwise = undefined -- /////

instance Show RealNum where
    showsPrec _ (NRatio r) =
        case (numerator r, denominator r) of
          (n, 1) -> shows n
          (n, m) -> shows n . (":" ++) . shows m
    showsPrec _ (NDouble d) = shows d

toDouble :: RealNum -> Double
toDouble (NRatio a) = fromRational a
toDouble (NDouble a) = a

instance Eq RealNum where
    NRatio a == NRatio b = a == b
    a == b = toDouble a == toDouble b

instance Ord RealNum where
    NRatio a `compare` NRatio b = a `compare` b
    a `compare` b = toDouble a `compare` toDouble b

instance Num RealNum where
    NRatio a + NRatio b = NRatio (a + b)
    a + b = NDouble (toDouble a + toDouble b)
    NRatio a * NRatio b = NRatio (a * b)
    a * b = NDouble (toDouble a * toDouble b)
    negate (NRatio a) = NRatio (negate a)
    negate (NDouble a) = NDouble (negate a)
    abs (NRatio a) = NRatio (abs a)
    abs (NDouble a) = NDouble (abs a)
    signum (NRatio a) = NRatio (signum a)
    signum (NDouble a) = NRatio (toRational $ signum a)
    fromInteger n = NRatio (fromInteger n)

instance Fractional RealNum where
    fromRational = NRatio
    recip (NRatio a) = NRatio (recip a)
    recip (NDouble a) = NDouble (recip a)

instance Floating RealNum where
    pi = NDouble pi
    exp   a = NDouble $ exp   (toDouble a)
    log   a = NDouble $ log   (toDouble a)
    sin   a = NDouble $ sin   (toDouble a)
    cos   a = NDouble $ cos   (toDouble a)
    asin  a = NDouble $ asin  (toDouble a)
    acos  a = NDouble $ acos  (toDouble a)
    atan  a = NDouble $ atan  (toDouble a)
    sinh  a = NDouble $ sinh  (toDouble a)
    cosh  a = NDouble $ sinh  (toDouble a)
    asinh a = NDouble $ asinh (toDouble a)
    acosh a = NDouble $ acosh (toDouble a)
    atanh a = NDouble $ atanh (toDouble a)

instance Real RealNum where
    toRational (NRatio a) = a
    toRational (NDouble a) = toRational a

instance RealFrac RealNum where
    properFraction (NRatio a) = second NRatio $ properFraction a
    properFraction (NDouble a) = second NDouble $ properFraction a

instance RealFloat RealNum where
    floatRadix = floatRadix . toDouble
    floatDigits = floatDigits . toDouble
    floatRange = floatRange . toDouble
    decodeFloat = decodeFloat . toDouble
    encodeFloat n m = NDouble $ encodeFloat n m
    isNaN (NRatio _) = False
    isNaN (NDouble a) = isNaN a
    isInfinite (NRatio _) = False
    isInfinite (NDouble a) = isInfinite a
    isDenormalized = isDenormalized . toDouble
    isNegativeZero (NRatio _) = False
    isNegativeZero (NDouble a) = isNegativeZero a
    isIEEE = isIEEE . toDouble
