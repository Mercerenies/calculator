
module Data.Calc.Function.Unit(uconvert) where

import Data.Calc.Unit.Table
import Data.Calc.Unit.Type
import Data.Calc.Unit.Parse
import Data.Calc.Function.Type
import Data.Calc.Util
import Data.Calc.Expr

import Prelude hiding (fail)
import Control.Monad.Reader hiding (fail)
import Control.Monad.Fail

data ConvertArgForm = ArgsImplicit | ArgsExplicit
                      deriving (Show, Read, Eq, Ord, Enum)

-- To clarify, uconvert accepts two forms.
--
-- Explicit arguments:
--
--   uconvert(3, in, ft)
--
-- This will convert 3 from inches to feet and produce a number as a
-- result.
--
-- Implicit arguments:
--
--   uconvert(3 * in, ft)
--
-- This will do the same, but rather than simply producing the result
-- N as a number, it will return N * ft (keeping the units tagged with
-- the number).

uconvert :: Monad m => Function m
uconvert = function "uconvert" go
    where go = do
            (form, expr, old, new) <- ask >>= parseArgs
            old' <- parseUnits table old
            new' <- parseUnits table new
            result <- maybeToFail $ convert old' new' expr
            return $ case form of
                       ArgsExplicit -> result
                       -- If arguments are passed "implicitly", include the
                       -- resulting units in the result
                       ArgsImplicit -> Compound "*" [result, new]
          parseArgs [expr, old, new] = pure (ArgsExplicit, expr, old, new)
          parseArgs [old, new] = pure (ArgsImplicit, Constant (PrimNum 1), old, new)
          parseArgs _ = fail "invalid args to uconvert"
