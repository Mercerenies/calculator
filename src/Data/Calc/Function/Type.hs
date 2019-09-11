{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs #-}

module Data.Calc.Function.Type(Function(..), functionSynonym) where

import Data.Calc.Expr
import Data.Calc.Mode

import Control.Monad.Reader

newtype Function where
    Function :: (forall m. MonadReader ModeInfo m => [Expr Prim] -> m (Maybe (Expr Prim))) -> Function

functionSynonym :: String -> Function
functionSynonym newname = Function (pure . Just . Compound newname)
