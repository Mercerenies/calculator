{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function(Function(..), functionSynonym,
                          stdBuiltins,
                          applyTo, applyToStd) where

import Data.Calc.Expr
import Data.Calc.Mode
import Data.Calc.Function.Type
import Data.Calc.Function.Approximate
import qualified Data.Calc.Function.Trigonometry as Trig
import qualified Data.Calc.Function.Transcendental as Trans
import qualified Data.Calc.Function.Combinatorial as Comb
import qualified Data.Calc.Function.Unit as Unit
import qualified Data.Calc.Function.Arithmetic as Arith
import qualified Data.Calc.Function.Vector as Vec
import Data.Calc.Calculus.Derivative

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Arrow

compileFns :: [Function m] -> Map String (Function m)
compileFns = fmap (fnName &&& id) >>> Map.fromList

stdBuiltins :: MonadReader ModeInfo m => Map String (Function m)
stdBuiltins = compileFns [
            Trig.fsin, Trig.fcos, Trig.ftan,
            Trig.fcsc, Trig.fsec, Trig.fcot,
            Trig.fasin, Trig.facos, Trig.fatan,
            Trig.facsc, Trig.fasec, Trig.facot,
            Trig.fsinh, Trig.fcosh, Trig.ftanh,
            Trig.fcsch, Trig.fsech, Trig.fcoth,
            Trig.fasinh, Trig.facosh, Trig.fatanh,
            Trig.facsch, Trig.fasech, Trig.facoth,
            functionSynonym "log" "ln",
            Trans.flog, Trans.fexp, Trans.fsqrt,
            Comb.ffact, Comb.fdfact, Comb.fncr, Comb.fnpr, Comb.fgcd, Comb.flcm,
            Arith.fabs,
            Vec.fdet, Vec.ftr, Vec.ftrn,
            approx stdBuiltins,
            derivativeFn stdBuiltins,
            Unit.uconvert, Unit.utconvert, Unit.ucanon
           ]

applyToStd :: MonadReader ModeInfo m => String -> [Expr Prim] -> m (Expr Prim)
applyToStd = applyTo stdBuiltins
