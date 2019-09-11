{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Data.Calc.Function.Trigonometry(fsin, fcos, ftan,
                                       fcsc, fsec, fcot,
                                       fasin, facos, fatan,
                                       facsc, fasec, facot,
                                       fsinh, fcosh, ftanh,
                                       fcsch, fsech, fcoth,
                                       fasinh, facosh, fatanh,
                                       facsch, fasech, facoth) where

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

fcsc :: Function
fcsc = function "csc" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . sin) . thetaToRad)

fsec :: Function
fsec = function "sec" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . cos) . thetaToRad)

fcot :: Function
fcot = function "cot" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . tan) . thetaToRad)

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

facsc :: Function
facsc = function "acsc" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (asin . recip))

fasec :: Function
fasec = function "asec" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (acos . recip))

facot :: Function
facot = function "acot" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (atan . recip))

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

fcsch :: Function
fcsch = function "csch" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . sinh) . thetaToRad)

fsech :: Function
fsech = function "sech" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . cosh) . thetaToRad)

fcoth :: Function
fcoth = function "coth" f
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . tanh) . thetaToRad)

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

facsch :: Function
facsch = function "acsch" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (asinh . recip))

fasech :: Function
fasech = function "asech" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (acosh . recip))

facoth :: Function
facoth = function "acoth" f
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (atanh . recip))
