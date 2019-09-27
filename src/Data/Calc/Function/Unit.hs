{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Unit(uconvert, ucanon) where

import Data.Calc.Unit.Table
import Data.Calc.Unit.Type
import Data.Calc.Unit.Parse(parseUnits)
import Data.Calc.Function.Type
import Data.Calc.Util
import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode

import Prelude hiding (id, (.), fail)
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

-- ucanon(expr)
ucanon :: MonadReader ModeInfo m => Function m
ucanon = function "ucanon" go
    where go = do
            [expr] <- ask
            lift . lift $ runPassOnceBUM canonicalizePass expr
          canonicalizePass = PassT canonicalize
          canonicalize x =
              case parseUnits table x of
                Nothing -> pure x
                Just x' -> let (e, u) = canonical $ unitDim x'
                           in case convert x' u (Constant (PrimNum 1)) of
                                Nothing -> pure x
                                Just x'' -> pure $ Compound "*" [x'', e]
