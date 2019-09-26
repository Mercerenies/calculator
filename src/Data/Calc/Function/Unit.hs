
module Data.Calc.Function.Unit where

import Data.Calc.Unit.Table
import Data.Calc.Unit.Type
import Data.Calc.Unit.Parse
import Data.Calc.Function.Type
import Data.Calc.Util
import Data.Calc.Expr

import Prelude hiding (fail)
import Control.Monad.Reader hiding (fail)
import Control.Monad.Fail

-- This is a cheap and dirty covert function. It's just for me to test
-- things right now.
--
-- TODO Clean it up and put it somewhere under Data.Calc.Function.*
uconvert :: Monad m => Function m
uconvert = function "uconvert" go
    where go = do
            (expr, old, new) <- ask >>= parseArgs
            old' <- parseUnits table old
            new' <- parseUnits table new
            maybeToFail $ convert old' new' expr
          parseArgs [expr, old, new] = pure (expr, old, new)
          parseArgs [old, new] = pure (Constant (PrimNum 1), old, new)
          parseArgs _ = fail "invalid args to uconvert"
