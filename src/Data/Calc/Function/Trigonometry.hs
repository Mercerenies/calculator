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
ftan = function "tan" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap tan . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "^" [Compound "sec" [x],
                                                                       Constant (PrimNum 2)]))

fcsc :: Function
fcsc = function "csc" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . sin) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "csc" [x],
                                                                        Compound "cot" [x]]
                                                         ]))

fsec :: Function
fsec = function "sec" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . cos) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "*" [Compound "csc" [x],
                                                                       Compound "cot" [x]]))

fcot :: Function
fcot = function "cot" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . tan) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "^" [Compound "csc" [x],
                                                                        Constant (PrimNum 2)]
                                                         ]))

fasin :: Function
fasin = function "asin" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asin)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "sqrt" [
                                                           Compound "-" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

facos :: Function
facos = function "acos" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acos)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "sqrt" [
                                                            Compound "-" [
                                                             Constant (PrimNum 1),
                                                             Compound "^" [x, Constant (PrimNum 2)]
                                                            ]
                                                           ]
                                                          ]
                                                         ]))

fatan :: Function
fatan = function "atan" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atan)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "+" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))

facsc :: Function
facsc = function "acsc" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (asin . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "*" [
                                                            Compound "sqrt" [
                                                             Compound "-" [
                                                              Constant (PrimNum 1),
                                                              Compound "/" [
                                                               Constant (PrimNum 1),
                                                               Compound "^" [x, Constant (PrimNum 2)]
                                                              ]
                                                             ]
                                                            ],
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

fasec :: Function
fasec = function "asec" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (acos . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "*" [
                                                           Compound "sqrt" [
                                                            Compound "-" [
                                                             Constant (PrimNum 1),
                                                             Compound "/" [
                                                              Constant (PrimNum 1),
                                                              Compound "^" [x, Constant (PrimNum 2)]
                                                             ]
                                                            ]
                                                           ],
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))

facot :: Function
facot = function "acot" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (atan . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "+" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

fsinh :: Function
fsinh = function "sinh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap sinh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "cosh" [x]))

fcosh :: Function
fcosh = function "cosh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap cosh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "sinh" [x]))

ftanh :: Function
ftanh = function "tanh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap tanh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "^" [Compound "sech" [x],
                                                                       Constant (PrimNum 2)]))

fcsch :: Function
fcsch = function "csch" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . sinh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "csch" [x],
                                                                        Compound "coth" [x]]
                                                         ]))

fsech :: Function
fsech = function "sech" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . cosh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "sech" [x],
                                                                        Compound "tanh" [x]]
                                                         ]))

fcoth :: Function
fcoth = function "coth" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (fmap (recip . tanh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "^" [Compound "csch" [x],
                                                                        Constant (PrimNum 2)]
                                                         ]))

fasinh :: Function
fasinh = function "asinh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . asinh)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "sqrt" [
                                                           Compound "+" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

facosh :: Function
facosh = function "acosh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . acosh)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "*" [
                                                           Compound "sqrt" [
                                                            Compound "+" [x, Constant (PrimNum 1)]
                                                           ],
                                                           Compound "sqrt" [
                                                            Compound "-" [x, Constant (PrimNum 1)]
                                                           ]
                                                          ]
                                                         ]))

fatanh :: Function
fatanh = function "atanh" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . atanh)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "-" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))

facsch :: Function
facsch = function "acsch" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (asinh . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "*" [
                                                            Compound "sqrt" [
                                                             Compound "+" [
                                                              Constant (PrimNum 1),
                                                              Compound "/" [
                                                               Constant (PrimNum 1),
                                                               Compound "^" [x, Constant (PrimNum 2)]
                                                              ]
                                                             ]
                                                            ],
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

fasech :: Function
fasech = function "asech" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (acosh . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "*" [
                                                            Compound "sqrt" [
                                                             Compound "+" [
                                                              Compound "/" [
                                                               Constant (PrimNum 1),
                                                               x
                                                              ],
                                                              Constant (PrimNum 1)
                                                             ]
                                                            ],
                                                            Compound "sqrt" [
                                                             Compound "-" [
                                                              Compound "/" [
                                                               Constant (PrimNum 1),
                                                               x
                                                              ],
                                                              Constant (PrimNum 1)
                                                             ]
                                                            ],
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

facoth :: Function
facoth = function "acoth" f `withDeriv` inOneVar f'
    where f :: FunctionType
          f = simpleUnaryFn (radToTheta . (atanh . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "-" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))
