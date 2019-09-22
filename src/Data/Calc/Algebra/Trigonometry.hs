{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}

module Data.Calc.Algebra.Trigonometry where

import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Number
import Data.Calc.Mode
import Data.Calc.Unit.Radians
import Data.Calc.Util

import Prelude hiding (fail, (.), id)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Ratio
import Control.Monad.Reader hiding (fail)
import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Control.Arrow

data TrigData = TrigData {
      trigName :: String,
      trigTable :: Map Integer (Expr Prim),
      trigPeriod :: Maybe Integer
    } deriving (Show)

negated :: Expr a -> Expr a
negated x = Compound "_" [x]

knownSinDeg :: Map Integer (Expr Prim)
knownSinDeg =
    Map.fromList [
        (0, Constant $ PrimNum 0),
        (30, Constant $ PrimNum (NRatio $ 1 % 2)),
        (45, Compound "/" [Compound "sqrt" [Constant $ PrimNum 2], Constant $ PrimNum 2]),
        (60, Compound "/" [Compound "sqrt" [Constant $ PrimNum 3], Constant $ PrimNum 2]),
        (90, Constant $ PrimNum 1),
        (120, Compound "/" [Compound "sqrt" [Constant $ PrimNum 3], Constant $ PrimNum 2]),
        (135, Compound "/" [Compound "sqrt" [Constant $ PrimNum 2], Constant $ PrimNum 2]),
        (150, Constant $ PrimNum (NRatio $ 1 % 2)),
        (180, Constant $ PrimNum 0),
        (210, negated . Constant $ PrimNum (NRatio $ 1 % 2)),
        (225, negated $ Compound "/" [Compound "sqrt" [Constant $ PrimNum 2], Constant $ PrimNum 2]),
        (240, negated $ Compound "/" [Compound "sqrt" [Constant $ PrimNum 3], Constant $ PrimNum 2]),
        (270, negated . Constant $ PrimNum 1),
        (300, negated $ Compound "/" [Compound "sqrt" [Constant $ PrimNum 3], Constant $ PrimNum 2]),
        (315, negated $ Compound "/" [Compound "sqrt" [Constant $ PrimNum 2], Constant $ PrimNum 2]),
        (330, negated . Constant $ PrimNum (NRatio $ 1 % 2))
    ]

knownCosDeg :: Map Integer (Expr Prim)
knownCosDeg = Map.mapKeys shift knownSinDeg
    where shift n = (n - 90) `mod` 360

compileTrigData :: [TrigData] -> Map String TrigData
compileTrigData = fmap (trigName &&& id) >>> Map.fromList

knownTrigData :: Map String TrigData
knownTrigData =
    compileTrigData [
        TrigData "sin" knownSinDeg (Just 360),
        TrigData "cos" knownCosDeg (Just 360)
    ]

knownTrigSynonyms :: Map String (Expr Prim -> Expr Prim)
knownTrigSynonyms =
    Map.fromList [
        ("tan", \x -> Compound "/" [Compound "sin" [x], Compound "cos" [x]]),
        ("sec", \x -> Compound "/" [Constant (PrimNum 1), Compound "cos" [x]]),
        ("csc", \x -> Compound "/" [Constant (PrimNum 1), Compound "sin" [x]]),
        ("cot", \x -> Compound "/" [Compound "cos" [x], Compound "sin" [x]])
    ]

simpleSolve :: (MonadFail m, MonadReader ModeInfo m) =>
               PassT m Prim Prim -> TrigData -> Expr Prim -> m (Expr Prim)
simpleSolve inner (TrigData {..}) x = do
  -- TODO At some point I'd like to automate this "convert unit X to
  -- unit Y" thing, but for now we need to explicitly convert from X
  -- to radians then to Y.
  factor1 <- thetaToRadFactorSym
  factor2 <- local (\mode -> mode { angularMode = Degrees }) radToThetaFactorSym
  Constant (PrimNum (NRatio x')) <- runPassTDM inner (Compound "*" [factor1, factor2, x])
  unless (denominator x' == 1) $ fail "can't lookup non-integer degree amounts"
  let value = case trigPeriod of
                Nothing -> numerator x'
                Just p  -> numerator x' `mod` p
  maybeToFail $ Map.lookup value trigTable

-- Fills in the values for the elementary trig functions, such as sin
-- and cos, from a table of known values.
simpleSolvePass :: MonadReader ModeInfo m => PassT m Prim Prim -> PassT m Prim Prim
simpleSolvePass inner = PassT go
    where go (Compound f [x]) | Just dat <- Map.lookup f knownTrigData =
              runMaybeT (simpleSolve (liftPassT inner) dat x) >>= \case
                        Nothing -> pure (Compound f [x])
                        Just x' -> pure x'
          go x = pure x

-- Solves "equivalent" trig functions, such as replacing tan with
-- sin/cos.
equivSolvePass :: MonadReader ModeInfo m => PassT m Prim Prim -> PassT m Prim Prim
equivSolvePass inner = PassT go
    where go (Compound f [x]) | Just repl <- Map.lookup f knownTrigSynonyms =
              runMaybeT (runPassTDM walk $ repl x) >>= \case
                        Nothing -> pure (Compound f [x])
                        Just x' -> pure x'
          go x = pure x
          walk = PassT walk'
          walk' (Compound f [x]) | Just dat <- Map.lookup f knownTrigData =
                 simpleSolve (liftPassT inner) dat x
          walk' x = pure x
