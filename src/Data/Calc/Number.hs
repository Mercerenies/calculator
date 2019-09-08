{-# LANGUAGE Rank2Types #-}

module Data.Calc.Number(Number(..), cmp, lexCmp) where

import Data.Ratio
import Data.Complex
import Data.Semigroup
import Control.Applicative

-- ///// Need to make asin, acos, etc. produce complex numbers if given out of range (for real) args

data Number = NRatio Rational
            | NDouble Double
            | NComplex (Complex Double)

instance Show Number where
    showsPrec _ (NRatio r) =
        case (numerator r, denominator r) of
          (n, 1) -> shows n
          (n, m) -> shows n . (":" ++) . shows m
    showsPrec _ (NDouble d) = shows d
    showsPrec _ (NComplex (a :+ b)) = shows (a, b) -- Show in tuple form

rToD :: Rational -> Double
rToD = fromRational

dToC :: Double -> Complex Double
dToC = (:+ 0)

rToC :: Rational -> Complex Double
rToC = dToC . rToD

--toR :: Number -> Maybe Rational
--toR (NRatio a) = Just a
--toR _ = Nothing

toD :: Number -> Maybe Double
toD (NRatio a) = Just (rToD a)
toD (NDouble a) = Just a
toD _ = Nothing

toC :: Number -> Complex Double
toC (NRatio a) = rToC a
toC (NDouble a) = dToC a
toC (NComplex a) = a

binaryPromote :: (forall a. (Fractional a, Eq a) => a -> a -> a) -> Number -> Number -> Number
binaryPromote f a b =
    case (a, b) of
      (NComplex a', NComplex b') -> NComplex $ f a' b'
      (NComplex a', NDouble  b') -> NComplex $ f a' (dToC b')
      (NComplex a', NRatio   b') -> NComplex $ f a' (rToC b')
      (NDouble  a', NComplex b') -> NComplex $ f (dToC a') b'
      (NRatio   a', NComplex b') -> NComplex $ f (rToC a') b'
      (NDouble  a', NDouble  b') -> NDouble  $ f a' b'
      (NDouble  a', NRatio   b') -> NDouble  $ f a' (rToD b')
      (NRatio   a', NDouble  b') -> NDouble  $ f (rToD a') b'
      (NRatio   a', NRatio   b') -> NRatio   $ f a' b'

unaryPromote :: (forall a. (Fractional a, Eq a) => a -> a) -> Number -> Number
unaryPromote f a =
    case a of
      NComplex a' -> NComplex $ f a'
      NDouble  a' -> NDouble  $ f a'
      NRatio   a' -> NRatio   $ f a'

floatingPromote :: (forall a. (Floating a, Eq a) => a -> a) -> Number -> Number
floatingPromote f a =
    case a of
      NComplex a' -> NComplex $ f a'
      NDouble  a' -> NDouble  $ f a'
      NRatio   a' -> NDouble  $ f (fromRational a')

binaryPromote' :: (forall a. (Fractional a, Eq a) => a -> a -> b) -> Number -> Number -> b
binaryPromote' f a b =
    case (a, b) of
      (NComplex a', NComplex b') -> f a' b'
      (NComplex a', NDouble  b') -> f a' (dToC b')
      (NComplex a', NRatio   b') -> f a' (rToC b')
      (NDouble  a', NComplex b') -> f (dToC a') b'
      (NRatio   a', NComplex b') -> f (rToC a') b'
      (NDouble  a', NDouble  b') -> f a' b'
      (NDouble  a', NRatio   b') -> f a' (rToD b')
      (NRatio   a', NDouble  b') -> f (rToD a') b'
      (NRatio   a', NRatio   b') -> f a' b'

instance Eq Number where
    (==) = binaryPromote' (==)

-- Refuses to compare if either argument is complex.
cmp :: Number -> Number -> Maybe Ordering
NRatio a `cmp` NRatio a' = Just (a `compare` a')
a `cmp` a' = liftA2 compare (toD a) (toD a')

-- "Lexicographic" comparison. Works like the derived Ord instance
-- would if Data.Complex had a (derived) Ord as well, ignoring the
-- usual rules of complex numbers.
lexCmp :: Number -> Number -> Ordering
lexCmp (NRatio a) (NRatio a') = a `compare` a'
lexCmp (NRatio _) _ = LT
lexCmp _ (NRatio _) = GT
lexCmp (NDouble a) (NDouble a') = a `compare` a'
lexCmp (NDouble _) _ = LT
lexCmp _ (NDouble _) = GT
lexCmp (NComplex (a :+ b)) (NComplex (a' :+ b')) = a `compare` a' <> b `compare` b'

pow :: Number -> Number -> Number
-- Deal with the "base" cases of zero.
pow _ 0 = NRatio 1
pow 0 _ = NRatio 0
-- If either argument is complex, so is the result.
pow (NComplex a) b = NComplex $ a ** (toC b)
pow a (NComplex b) = NComplex $ (toC a) ** b
-- Next, if the base is positive, we can keep the result real
pow (NRatio a) (NRatio b) | a > 0 && denominator b == 1 = NRatio $ a ^^ numerator b
pow (NRatio a) (NRatio b) | a > 0 = NDouble $ rToD a ** rToD b
pow (NDouble a) (NDouble b) | a > 0 = NDouble $ a ** b
pow (NRatio a) (NDouble b) | a > 0 = NDouble $ rToD a ** b
pow (NDouble a) (NRatio b) | a > 0 && denominator b == 1 = NDouble $ a ^^ numerator b
pow (NDouble a) (NRatio b) | a > 0 = NDouble $ a ** rToD b
-- Next, if the base is negative, we can only remain real if the exponent is an integer
pow (NRatio a) (NRatio b) | denominator b == 1 = NRatio $ a ^^ numerator b
pow (NDouble a) (NRatio b) | denominator b == 1 = NDouble $ a ^^ numerator b
-- Otherwise, go complex, as all bets are off
pow a b = NComplex $ toC a ** toC b

instance Num Number where
    (+) = binaryPromote (+)
    (*) = binaryPromote (*)
    negate = unaryPromote negate
    abs = unaryPromote abs
    signum (NRatio a) = NRatio (signum a)
    signum (NDouble a) = NRatio (toRational $ signum a) -- Should be +/- 1 or 0 so make it rational
    signum (NComplex a) = NComplex a -- Probably still complex so leave it
    fromInteger = NRatio . fromInteger

instance Fractional Number where
    recip = unaryPromote recip
    fromRational = NRatio

instance Floating Number where
    pi = NDouble pi
    (**) = pow
    exp   = floatingPromote exp
    log   = floatingPromote log
    sin   = floatingPromote sin
    cos   = floatingPromote cos
    asin  = floatingPromote asin
    acos  = floatingPromote acos
    atan  = floatingPromote atan
    sinh  = floatingPromote sinh
    cosh  = floatingPromote cosh
    asinh = floatingPromote asinh
    acosh = floatingPromote acosh
    atanh = floatingPromote atanh

{-
pow :: Number -> Rational -> Number
pow a b
    -- a^0 = 1 (Modulo issues with 0^0 I suppose...)
    | b == 0 = 1
    -- Integer, so it's just repeated multiplication
    | denominator b == 1 = a ^^ numerator b
    -- Rational non-integer, I can't help you
    | otherwise = NDouble $ toDouble a ** fromRational b

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
-}
