{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- I'm using this file for debugging things right now, so let's ignore
-- the incompleteness, eh?

-- I should mention I'm using this as a guide.
--
-- http://www.math.wpi.edu/IQP/BVCalcHist/calc5.html

module Main where

import Data.Calc.Expr
import Data.Calc.Print
import Data.Calc.Pass
import Data.Calc.Normalize

import Prelude hiding ((.), id)

-- (+ 3 (- 2 1) 4 (* 2 z) z (* z 3) (* x y (^ x 2)) (_ (* 2 9)))
example :: Expr Prim
example = Compound "+" [Constant (PrimInt 3),
                        Compound "-" [Constant (PrimInt 2), Constant (PrimInt 1)],
                        Constant (PrimInt 4),
                        Compound "*" [Constant (PrimInt 2), Constant (PrimVar "z")],
                        Constant (PrimVar "z"),
                        Compound "*" [Constant (PrimVar "z"), Constant (PrimInt 3)],
                        Compound "*" [
                         Constant (PrimVar "x"),
                         Constant (PrimVar "y"),
                         Compound "^" [Constant (PrimVar "x"), Constant (PrimInt 2)]
                        ],
                        Compound "_" [Compound "*" [Constant (PrimInt 2), Constant (PrimInt 9)]]]

example1 :: Expr Prim
example1 = Compound "^" [Constant (PrimVar "A"), Constant (PrimInt 1)]

example2 :: Expr Prim
example2 = Compound "+" [Constant (PrimVar "A"), Compound "+" [Constant (PrimVar "B"), Constant (PrimVar "C")]]

example3 :: Expr Prim
example3 = Compound "+" [Compound "+" [Constant (PrimVar "A"), Constant (PrimVar "B")], Constant (PrimVar "C")]

example4 :: Expr Prim
example4 = Compound "+" [Constant (PrimVar "A"), Constant (PrimVar "B"), Constant (PrimVar "C")]

main :: IO ()
main = do
  let pass = sortTermsOfStd . flattenStdSingletons . foldConstants . flattenStdSingletons . collectLikeTerms . collectLikeFactors . levelStdOperators . normalizeNegatives
  let pass1 = flattenStdSingletons . foldConstants . collectLikeTerms . collectLikeFactors . levelStdOperators . normalizeNegatives
  putStrLn "Hello :)"
  putStrLn $ lispLikeShow example4
  putStrLn $ lispLikeShow (runPassOnceTD pass1 $ example4)
--  putStrLn $ formulaShow (runPassTD pass1 $ example2)
--  putStrLn $ formulaShow (runPassTD pass $ example3)
  putStrLn $ formulaShow (runPassTD pass $ example)
  putStrLn $ lispLikeShow (runPassTD pass $ example)
