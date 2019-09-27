
module Data.Calc.Unit.Parse(parseUnits) where

import Data.Calc.Expr
import Data.Calc.Util
import Data.Calc.Unit.Type
import Data.Calc.Unit.Dimension(unitless)
import Data.Calc.Coerce

import Prelude hiding (fail, id, (.), div)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Fail
import Control.Category
import Control.Applicative

data NamedUnit b a = NamedUnit String (Unit b a)

data FractionalUnit b a = FractionalUnit (Unit b a) [NamedUnit b a] [NamedUnit b a]

one :: FractionalUnit a a
one = FractionalUnit id [] []

mul :: FractionalUnit a a -> FractionalUnit a a -> FractionalUnit a a
mul (FractionalUnit u a b) (FractionalUnit u' a' b') =
    FractionalUnit (u .* u') (a ++ a') (b ++ b')

div :: FractionalUnit a a -> FractionalUnit a a -> FractionalUnit a a
div (FractionalUnit u a b) (FractionalUnit u' a' b') =
    FractionalUnit (u ./ u') (a ++ b') (a' ++ b)

pow :: FractionalUnit a a -> Int -> FractionalUnit a a
pow (FractionalUnit u a b) n =
    FractionalUnit (u .^ n) (concat $ replicate n a) (concat $ replicate n b)

fracToUnit :: FractionalUnit a a -> Unit a a
fracToUnit (FractionalUnit u n d) = u .* (n' ./ d')
    where unNamedUnit (NamedUnit _ un) = un
          n' = foldl (.*) id $ map unNamedUnit n
          d' = foldl (.*) id $ map unNamedUnit d

--fracDim :: FractionalUnit a a -> Dimension
--fracDim = unitDim . fracToUnit

parseFracUnits :: MonadFail m => Map String (Unit (Expr Prim) (Expr Prim)) -> Expr Prim ->
                  m (FractionalUnit (Expr Prim) (Expr Prim))
parseFracUnits m = go
    where go (Constant (PrimVar x))
              | Just u <- lookup' x = pure $ FractionalUnit id [NamedUnit x u] []
              | otherwise = fail $ "unknown unit " ++ x
          go (x @ (Constant (PrimNum _))) =
              let unit = Unit unitless (\y -> Compound "*" [y, x]) (\y -> Compound "/" [y, x])
              in pure $ FractionalUnit unit [] []
          go (Compound "*" xs) = fmap (foldl mul one) $ mapM go xs
          go (Compound "/" [a, b]) = liftA2 div (go a) (go b)
          go (Compound "^" [a, b])
              | Constant (PrimNum n) <- b
              , Just n' <- fromInteger <$> coerceToInt n
              = (`pow` n') <$> go a
          go _ = fail "cannot parse unit"
          lookup' x = maybeToFail $ Map.lookup x m

parseUnits :: MonadFail m => Map String (Unit (Expr Prim) (Expr Prim)) -> Expr Prim ->
              m (Unit (Expr Prim) (Expr Prim))
parseUnits m e = fmap fracToUnit $ parseFracUnits m e
