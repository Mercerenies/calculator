{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Trigonometry where

import Data.Calc.Function.Type
import Data.Calc.Unit.Radians

-- ///// Derivatives (don't forget about degree / radian conversions)

fsin :: Function
fsin = function "sin" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap sin . thetaToRad)

fcos :: Function
fcos = function "cos" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap cos . thetaToRad)

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
