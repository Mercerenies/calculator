
module Data.Calc.Number(Number(..), RealNum(..), toDouble, dot, ii) where

import Data.Ratio
import Control.Arrow

data Number = Number RealNum RealNum -- Real and complex parts
              deriving (Eq)

data RealNum = NRatio Rational
             | NDouble Double

instance Show Number where
    showsPrec n (Number a b)
        | b == 0 = showsPrec n a
        | otherwise = showsPrec n (a, b) -- Show it in tuple form

instance Show RealNum where
    showsPrec _ (NRatio r) =
        case (numerator r, denominator r) of
          (n, 1) -> shows n
          (n, m) -> shows n . (":" ++) . shows m
    showsPrec _ (NDouble d) = shows d

toDouble :: RealNum -> Double
toDouble (NRatio a) = fromRational a
toDouble (NDouble a) = a

pow :: RealNum -> Rational -> RealNum
pow a b
    -- a^0 = 1 (Modulo issues with 0^0 I suppose...)
    | b == 0 = 1
    -- Integer, so it's just repeated multiplication
    | denominator b == 1 = a ^^ numerator b
    -- Rational non-integer, I can't help you
    | otherwise = NDouble $ toDouble a ** fromRational b

dot :: Number -> Number -> RealNum
dot (Number a b) (Number a' b') = a * a' + b * b'

ii :: Number
ii = Number 0 1

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
    _ ** 0 = 1
    a ** NRatio b = pow a b                    -- Try to keep it rational, if we can...
    a ** NDouble b = NDouble $ toDouble a ** b -- but we can't always.
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
{-
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
-}

instance Num Number where
    Number a b + Number a' b' = Number (a + a') (b + b')
    Number a b * Number a' b' = Number (a * a' - b * b') (a * b' + b * a')
    negate (Number a b) = Number (negate a) (negate b)
    abs x = sqrt $ Number (x `dot` x) 0
    -- Keep rational precision if possible
    signum (Number a 0) = Number (signum a) 0
    signum (Number 0 b) = Number 0 (signum b)
    signum x = x / abs x
    fromInteger a = Number (fromInteger a) 0

instance Fractional Number where
    fromRational q = Number (fromRational q) 0
    recip (Number a b) = Number (a / abs2) (- b / abs2)
        where abs2 = Number a b `dot` Number a b

instance Floating Number where
    pi = Number pi 0
    exp (Number a b) = Number (exp a * cos b) (- exp a * sin b)
    log (Number a b) = Number (log magn) (NDouble $ atan2 (toDouble b) (toDouble a))
        where Number magn _ = abs (Number a b)
    sin x = (exp (ii * x) - exp (- ii * x)) / (2 * ii)
    cos x = (exp (ii * x) + exp (- ii * x)) / 2
