{-# LANGUAGE DeriveFunctor #-}

module Data.Calc.Expr(Expr(..), Prim(..),
                      isConstant, isCompound) where

import Data.Calc.Repr

data Expr a = Constant a
            | Compound String [Expr a]
              deriving (Show, Eq, Ord, Functor)

data Prim = PrimInt Integer
          | PrimVar String
            deriving (Eq, Ord)

instance Show Prim where
    showsPrec n (PrimInt x) = showsPrec n x
    showsPrec _ (PrimVar x) = (x ++)

instance ReprInteger Prim where
    reprInteger = PrimInt

instance ReprInteger a => ReprInteger (Expr a) where
    reprInteger = Constant . reprInteger

instance HasVars Prim where
    isVar (PrimVar {}) = True
    isVar _ = False

instance HasNumbers Prim where
    isNumber (PrimInt {}) = True
    isNumber _ = False

isConstant :: Expr a -> Bool
isConstant (Constant {}) = True
isConstant _ = False

isCompound :: Expr a -> Bool
isCompound (Compound {}) = True
isCompound _ = False
