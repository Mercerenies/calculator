
module Data.Calc.Unit.Parse where

import Data.Calc.Expr
import Data.Calc.Util
import Data.Calc.Unit.Type
import Data.Calc.Coerce

import Prelude hiding (fail, id, (.))
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Fail
import Control.Category
import Control.Applicative

parseUnits :: MonadFail m => Map String (Unit (Expr Prim) (Expr Prim)) -> Expr Prim ->
              m (Unit (Expr Prim) (Expr Prim))
parseUnits m = go
    where go (Constant (PrimVar x))
              | Just u <- lookup' x = pure u
              | otherwise = fail $ "unknown unit " ++ x
          go (Compound "*" xs) = fmap (foldl (.*) id) $ mapM go xs
          go (Compound "/" [a, b]) = liftA2 (./) (go a) (go b)
          go (Compound "^" [a, b])
              | Constant (PrimNum n) <- b
              , Just n' <- fromInteger <$> coerceToInt n
              = (.^ n') <$> go a
          go _ = fail "cannot parse unit"
          lookup' x = maybeToFail $ Map.lookup x m
