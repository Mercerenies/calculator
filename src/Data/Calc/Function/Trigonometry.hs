{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Trigonometry where

import Data.Calc.Function.Type
import Data.Calc.Unit.Radians

fsin :: Function
fsin = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap sin . thetaToRad)

fcos :: Function
fcos = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap cos . thetaToRad)

ftan :: Function
ftan = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap tan . thetaToRad)

fasin :: Function
fasin = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asin)

facos :: Function
facos = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acos)

fatan :: Function
fatan = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atan)

fsinh :: Function
fsinh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap sinh . thetaToRad)

fcosh :: Function
fcosh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap cosh . thetaToRad)

ftanh :: Function
ftanh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (fmap tanh . thetaToRad)

fasinh :: Function
fasinh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asinh)

facosh :: Function
facosh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acosh)

fatanh :: Function
fatanh = Function f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atanh)
