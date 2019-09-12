{-# LANGUAGE DeriveFunctor #-}

module Data.Calc.Expr(Expr(..), Prim(..),
                      isConstant, isCompound) where

import Data.Calc.Repr
import Data.Calc.Number

data Expr a = Constant a
            | Compound String [Expr a]
              deriving (Show, Eq, Ord, Functor)

data Prim = PrimNum Number
          | PrimVar String
            deriving (Eq)

-- So we can't derive this instance since Number is fundamentally a
-- complex number, which Haskell won't let us order, but we need an
-- order, even if an arbitrary one, for canonicalization of terms.
instance Ord Prim where
    PrimVar a `compare` PrimVar b = a `compare` b
    PrimNum _ `compare` PrimVar _ = LT
    PrimVar _ `compare` PrimNum _ = GT
    PrimNum a `compare` PrimNum b = a `lexCmp` b

instance Show Prim where
    showsPrec n (PrimNum x) = showsPrec n x
    showsPrec _ (PrimVar x) = (x ++)

instance ReprInteger Prim where
    reprInteger = PrimNum . fromInteger

instance ReprInteger a => ReprInteger (Expr a) where
    reprInteger = Constant . reprInteger

instance HasVars Prim where
    isVar (PrimVar {}) = True
    isVar _ = False

instance HasNumbers Prim where
    isNumber (PrimNum {}) = True
    isNumber _ = False
    isPositive (PrimNum a) | a `cmp` 0 == Just GT = True
    isPositive _ = False
    isNegative (PrimNum a) | a `cmp` 0 == Just LT = True
    isNegative _ = False

isConstant :: Expr a -> Bool
isConstant (Constant {}) = True
isConstant _ = False

isCompound :: Expr a -> Bool
isCompound (Compound {}) = True
isCompound _ = False
