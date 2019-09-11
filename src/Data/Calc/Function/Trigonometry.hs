{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Data.Calc.Function.Trigonometry(fsin, fcos, ftan, fasin, facos, fatan,
                                       fsinh, fcosh, ftanh, fasinh, facosh, fatanh) where

import Data.Calc.Expr
import Data.Calc.Function.Type
import Data.Calc.Unit.Radians

import Control.Applicative

postmultiply :: Monad m => m (Expr a) -> m (Expr a) -> m (Expr a)
postmultiply = liftA2 (\a b -> Compound "*" [a, b])

fsin :: Function
fsin = function "sin" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap sin . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "cos" [x]))

fcos :: Function
fcos = function "cos" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap cos . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [Compound "sin" [x]]))

ftan :: Function
ftan = function "tan" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap tan . thetaToRad)

fasin :: Function
fasin = function "asin" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asin)

facos :: Function
facos = function "acos" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acos)

fatan :: Function
fatan = function "atan" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atan)

fsinh :: Function
fsinh = function "sinh" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap sinh . thetaToRad)

fcosh :: Function
fcosh = function "cosh" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap cosh . thetaToRad)

ftanh :: Function
ftanh = function "tanh" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap tanh . thetaToRad)

fasinh :: Function
fasinh = function "asinh" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asinh)

facosh :: Function
facosh = function "acosh" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acosh)

fatanh :: Function
fatanh = function "atanh" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atanh)
