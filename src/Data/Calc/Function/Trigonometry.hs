{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Trigonometry(fsin, fcos, ftan,
                                       fcsc, fsec, fcot,
                                       fasin, facos, fatan,
                                       facsc, fasec, facot,
                                       fsinh, fcosh, ftanh,
                                       fcsch, fsech, fcoth,
                                       fasinh, facosh, fatanh,
                                       facsch, fasech, facoth) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type
import Data.Calc.Unit.Radians

import Control.Applicative
import Control.Monad.Reader

postmultiply :: Monad m => m (Expr a) -> m (Expr a) -> m (Expr a)
postmultiply = liftA2 (\a b -> Compound "*" [a, b])

fsin :: MonadReader ModeInfo m => Function m
fsin = function "sin" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap sin . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "cos" [x]))

fcos :: MonadReader ModeInfo m => Function m
fcos = function "cos" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap cos . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [Compound "sin" [x]]))

ftan :: MonadReader ModeInfo m => Function m
ftan = function "tan" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap tan . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "^" [Compound "sec" [x],
                                                                       Constant (PrimNum 2)]))

fcsc :: MonadReader ModeInfo m => Function m
fcsc = function "csc" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . sin) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "csc" [x],
                                                                        Compound "cot" [x]]
                                                         ]))

fsec :: MonadReader ModeInfo m => Function m
fsec = function "sec" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . cos) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "*" [Compound "csc" [x],
                                                                       Compound "cot" [x]]))

fcot :: MonadReader ModeInfo m => Function m
fcot = function "cot" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . tan) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "^" [Compound "csc" [x],
                                                                        Constant (PrimNum 2)]
                                                         ]))

fasin :: MonadReader ModeInfo m => Function m
fasin = function "asin" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . asin)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "sqrt" [
                                                           Compound "-" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

facos :: MonadReader ModeInfo m => Function m
facos = function "acos" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . acos)
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

fatan :: MonadReader ModeInfo m => Function m
fatan = function "atan" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . atan)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "+" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))

facsc :: MonadReader ModeInfo m => Function m
facsc = function "acsc" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (asin . recip))
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

fasec :: MonadReader ModeInfo m => Function m
fasec = function "asec" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (acos . recip))
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

facot :: MonadReader ModeInfo m => Function m
facot = function "acot" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (atan . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "_" [
                                                          Compound "/" [
                                                           Constant (PrimNum 1),
                                                           Compound "+" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

fsinh :: MonadReader ModeInfo m => Function m
fsinh = function "sinh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap sinh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "cosh" [x]))

fcosh :: MonadReader ModeInfo m => Function m
fcosh = function "cosh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap cosh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "sinh" [x]))

ftanh :: MonadReader ModeInfo m => Function m
ftanh = function "tanh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap tanh . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "^" [Compound "sech" [x],
                                                                       Constant (PrimNum 2)]))

fcsch :: MonadReader ModeInfo m => Function m
fcsch = function "csch" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . sinh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "csch" [x],
                                                                        Compound "coth" [x]]
                                                         ]))

fsech :: MonadReader ModeInfo m => Function m
fsech = function "sech" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . cosh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "*" [Compound "sech" [x],
                                                                        Compound "tanh" [x]]
                                                         ]))

fcoth :: MonadReader ModeInfo m => Function m
fcoth = function "coth" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (fmap (recip . tanh) . thetaToRad)
          f' x = postmultiply thetaToRadFactorSym (pure (Compound "_" [
                                                          Compound "^" [Compound "csch" [x],
                                                                        Constant (PrimNum 2)]
                                                         ]))

fasinh :: MonadReader ModeInfo m => Function m
fasinh = function "asinh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . asinh)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "sqrt" [
                                                           Compound "+" [
                                                            Constant (PrimNum 1),
                                                            Compound "^" [x, Constant (PrimNum 2)]
                                                           ]
                                                          ]
                                                         ]))

facosh :: MonadReader ModeInfo m => Function m
facosh = function "acosh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . acosh)
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

fatanh :: MonadReader ModeInfo m => Function m
fatanh = function "atanh" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . atanh)
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "-" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))

facsch :: MonadReader ModeInfo m => Function m
facsch = function "acsch" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (asinh . recip))
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

fasech :: MonadReader ModeInfo m => Function m
fasech = function "asech" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (acosh . recip))
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

facoth :: MonadReader ModeInfo m => Function m
facoth = function "acoth" f `withDeriv` inOneVar f'
    where f = simpleUnaryFn (radToTheta . (atanh . recip))
          f' x = postmultiply radToThetaFactorSym (pure (Compound "/" [
                                                          Constant (PrimNum 1),
                                                          Compound "-" [
                                                           Constant (PrimNum 1),
                                                           Compound "^" [x, Constant (PrimNum 2)]
                                                          ]
                                                         ]))
