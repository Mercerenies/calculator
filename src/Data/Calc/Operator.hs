
module Data.Calc.Operator where

import Data.Map(Map)
import qualified Data.Map as Map

data Op = Op String Int Assoc Fixity
          deriving (Show, Read, Eq, Ord)

data Assoc = NoAssoc | LeftAssoc | RightAssoc
             deriving (Show, Read, Eq, Ord, Enum)

data Fixity = Infix | Prefix | Postfix
             deriving (Show, Read, Eq, Ord, Enum)

stdOps :: Map String Op
stdOps = Map.fromList [
          ("+", Op "+" 6 LeftAssoc Infix),
          ("-", Op "-" 6 LeftAssoc Infix),
          ("*", Op "*" 7 LeftAssoc Infix),
          ("/", Op "/" 7 LeftAssoc Infix),
          ("_", Op "-" 6 LeftAssoc Prefix)
         ]

getStdOp :: String -> Maybe Op
getStdOp s = Map.lookup s stdOps
