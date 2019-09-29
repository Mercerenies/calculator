
module Data.Calc.Shape.Type where

-- Necessary to have this in a different file from the other shape
-- stuff to avoid a cyclic module dependency between Data.Calc.Shape
-- and Data.Calc.Function.Type.

data Shape = Scalar | Vector | Matrix | Variable | Unknown
             deriving (Show, Read, Eq, Ord, Enum)
