
module Data.Calc.Function.Shape(Shape(..),
                                noShape, always, matchFirstArg) where

import Data.Calc.Function.Type
import Data.Calc.Shape.Type

noShape :: FunctionShape
noShape = always Unknown

always :: Shape -> FunctionShape
always s _ _ = s

matchFirstArg :: FunctionShape
matchFirstArg _ [] = Unknown
matchFirstArg f (x:_) = f x
