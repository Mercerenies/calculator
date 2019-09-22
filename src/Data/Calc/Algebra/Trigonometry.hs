{-# LANGUAGE FlexibleContexts, LambdaCase #-}

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
      trigTable :: Map Integer (Expr Prim)
    }

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
        TrigData "sin" knownSinDeg,
        TrigData "cos" knownCosDeg
    ]

simpleSolve :: (MonadFail m, MonadReader ModeInfo m) =>
               PassT m Prim Prim -> Map Integer (Expr Prim) -> Expr Prim -> m (Expr Prim)
simpleSolve inner m x = do
  -- TODO At some point I'd like to automate this "convert unit X to
  -- unit Y" thing, but for now we need to explicitly convert from X
  -- to radians then to Y.
  factor1 <- thetaToRadFactorSym
  factor2 <- local (\mode -> mode { angularMode = Degrees }) radToThetaFactorSym
  Constant (PrimNum (NRatio x')) <- runPassTDM inner (Compound "*" [factor1, factor2, x])
  unless (denominator x' == 1) $ fail "can't lookup non-integer degree amounts"
  maybeToFail $ Map.lookup (numerator x') m

simpleSolvePass :: MonadReader ModeInfo m => PassT m Prim Prim -> PassT m Prim Prim
simpleSolvePass inner = PassT go
    where go (Compound f [x]) | Just td <- Map.lookup f knownTrigData =
              runMaybeT (simpleSolve (liftPassT inner) (trigTable td) x) >>= \case
                        Nothing -> pure (Compound f [x])
                        Just x' -> pure x'
          go x = pure x
