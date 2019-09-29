{-# LANGUAGE FlexibleContexts #-}

module Data.Calc.Function.Unit(uconvert, utconvert, ucanon) where

import Data.Calc.Unit.Table
import Data.Calc.Unit.Type
import Data.Calc.Unit.Temperature
import Data.Calc.Unit.Parse(parseUnits)
import Data.Calc.Function.Type
import Data.Calc.Function.Shape
import Data.Calc.Util
import Data.Calc.Expr
import Data.Calc.Pass
import Data.Calc.Mode

import Prelude hiding (id, (.), fail)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (fail)
import Control.Monad.Fail
import Control.Applicative

data ConvertArgForm = ArgsImplicit | ArgsExplicit
                      deriving (Show, Read, Eq, Ord, Enum)

parseArgs :: MonadFail m => [Expr Prim] -> m (ConvertArgForm, Expr Prim, Expr Prim, Expr Prim)
parseArgs [expr, old, new] = pure (ArgsExplicit, expr, old, new)
parseArgs [old, new] = pure (ArgsImplicit, Constant (PrimNum 1), old, new)
parseArgs _ = fail "invalid args to uconvert"

-- TODO Should the unit functions do... something when given vector
-- arguments?

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
uconvert = function "uconvert" go `withShape` always Scalar
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

-- Same formats as uconvert but only works with one-dimensional
-- temperature units. Treats temperatures as absolute rather than
-- relative.
utconvert :: Monad m => Function m
utconvert = function "utconvert" go `withShape` always Scalar
    where go = do
            (form, expr, old, Constant (PrimVar new)) <- ask >>= parseArgs
            -- Right now, we're accepting very limited argument forms.
            -- The "old" expressions MUST be either (1) a simple
            -- temperature unit, or (2) a simple temperature unit
            -- multiplied by an expression. The "new" must be a single
            -- temperature unit.
            (n, old') <- extractTemp old
            new' <- maybeToFail $ Map.lookup new tempTable
            let expr' = Compound "*" [n, expr]
                result = tempConvert old' new' expr'
            return $ case form of
                       ArgsExplicit -> result
                       ArgsImplicit -> Compound "*" [result, Constant (PrimVar new)]
          extractSimpleTemp (Constant (PrimVar a)) = maybeToFail $ Map.lookup a tempTable
          extractSimpleTemp _ = fail "could not parse temperature"
          extractTemp (a @ (Constant (PrimVar _))) =
              fmap ((,) (Constant (PrimNum 1))) $ extractSimpleTemp a
          extractTemp (Compound "*" [a, b]) =
              (fmap ((,) a) $ extractSimpleTemp b) <|>
              (fmap ((,) b) $ extractSimpleTemp a)
          extractTemp _ = fail "could not parse temperature"

-- ucanon(expr)
ucanon :: MonadReader ModeInfo m => Function m
ucanon = function "ucanon" go `withShape` always Scalar
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
