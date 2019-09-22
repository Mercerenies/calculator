{-# LANGUAGE Rank2Types #-}

module Data.Calc.Number(Number(..), isRational, cmp, lexEq, lexCmp) where

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

binaryFold :: (Rational -> Rational -> b) ->
              (Double -> Double -> b) ->
              (Complex Double -> Complex Double -> b) ->
              (Number -> Number -> b)
binaryFold f g h a b =
    case (a, b) of
      (NComplex a', NComplex b') -> h a' b'
      (NComplex a', NDouble  b') -> h a' (dToC b')
      (NComplex a', NRatio   b') -> h a' (rToC b')
      (NDouble  a', NComplex b') -> h (dToC a') b'
      (NRatio   a', NComplex b') -> h (rToC a') b'
      (NDouble  a', NDouble  b') -> g a' b'
      (NDouble  a', NRatio   b') -> g a' (rToD b')
      (NRatio   a', NDouble  b') -> g (rToD a') b'
      (NRatio   a', NRatio   b') -> f a' b'


binaryPromote :: (forall a. (Fractional a, Eq a) => a -> a -> a) -> Number -> Number -> Number
binaryPromote f = binaryFold (fmap NRatio . f) (fmap NDouble . f) (fmap NComplex . f)

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

--binaryPromote' :: (forall a. (Fractional a, Eq a) => a -> a -> b) -> Number -> Number -> b
--binaryPromote' f = binaryFold f f f

instance Eq Number where
    -- The various simplifications will choke if an expression can't
    -- be equal to itself, because the ideal normalization is one that
    -- can get to a normal form, i.e. a form where applying the
    -- normalization produces a value equal to the former. We can't do
    -- that if equality is not reflexive, hence we need to circumvent
    -- the IEEE notion that NaN != NaN here.
    (==) = binaryFold rEq dEq cEq
        where rEq a a' = (a == a')
              dEq a a' = (a == a') || (isNaN a && isNaN a')
              cEq (a :+ b) (a' :+ b') = (a `dEq` a') && (b `dEq` b')

isRational :: Number -> Bool
isRational (NRatio _) = True
isRational _ = False

-- Refuses to compare if either argument is complex.
cmp :: Number -> Number -> Maybe Ordering
NRatio a `cmp` NRatio a' = Just (a `compare` a')
a `cmp` a' = liftA2 compare (toD a) (toD a')

-- Strict equality, arguments of different precision (e.g. a fraction
-- and an equivalent floating point) will never compare equal under
-- this comparison. The ordering is arbitrary but consistent
lexEq :: Number -> Number -> Bool
lexEq a b = lexCmp a b == EQ

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
    recip (NRatio a)
        | a == 0    = recip (NDouble 0)
        | otherwise = NRatio $ recip a
    recip (NDouble a) = NDouble $ recip a
    recip (NComplex a) = NComplex $ recip a
    fromRational = NRatio

instance Floating Number where
    pi = NDouble pi
    (**) = pow
    exp   = floatingPromote exp
    log (NRatio a) = log (NDouble $ fromRational a)
    log (NDouble a) | a >= 0 = NDouble $ log a
    log b = NComplex . log $ toC b
    sin   = floatingPromote sin
    cos   = floatingPromote cos
    asin (NRatio a) = asin (NDouble $ fromRational a)
    asin (NDouble a) | a >= -1 && a <= 1 = NDouble $ asin a
    asin b = NComplex . asin $ toC b
    acos (NRatio a) = acos (NDouble $ fromRational a)
    acos (NDouble a) | a >= -1 && a <= 1 = NDouble $ acos a
    acos b = NComplex . acos $ toC b
    atan  = floatingPromote atan
    sinh  = floatingPromote sinh
    cosh  = floatingPromote cosh
    asinh = floatingPromote asinh
    acosh (NRatio a) = acosh (NDouble $ fromRational a)
    acosh (NDouble a) | a >= 1 = NDouble $ acosh a
    acosh b = NComplex . acosh $ toC b
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
