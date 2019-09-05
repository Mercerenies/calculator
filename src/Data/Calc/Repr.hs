{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Calc.Repr(ReprInteger(..), WrappedNum(..),
                      HasVars(..), HasNumbers(..)) where

import Data.Semigroup

class ReprInteger a where
    reprInteger :: Integer -> a

newtype WrappedNum a =
    WrapNum { unwrapNum :: a }
    deriving (Show, Read, Eq, Num, Semigroup, Monoid, Ord)

instance Num a => ReprInteger (WrappedNum a) where
    reprInteger = WrapNum . fromInteger

instance ReprInteger Integer where
    reprInteger = id

class HasVars a where
    isVar :: a -> Bool

class HasNumbers a where
    isNumber :: a -> Bool
