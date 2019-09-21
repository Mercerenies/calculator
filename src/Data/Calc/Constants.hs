
module Data.Calc.Constants where

import Data.Calc.Expr
import Data.Calc.Number

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Complex
import Control.Arrow

data ConstValue a = ConstValue {
      constName :: String,
      constValue :: a,
      constExact :: Bool
    } deriving (Show, Read, Eq)

compileConsts :: [ConstValue a] -> Map String (ConstValue a)
compileConsts = fmap (constName &&& id) >>> Map.fromList

stdConstants :: Map String (ConstValue Prim)
stdConstants = compileConsts [
                ConstValue "pi" (PrimNum pi) False,
                ConstValue "e" (PrimNum (exp 1)) False,
                ConstValue "i" (PrimNum (NComplex (0 :+ 1))) False -- TODO Is this "exact"?
               ]
