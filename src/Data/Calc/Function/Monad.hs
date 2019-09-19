{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving,
  FlexibleInstances, MultiParamTypeClasses #-}

module Data.Calc.Function.Monad where

--import Data.Calc.Function.Type
import Data.Calc.Expr
import Data.Calc.Mode
--import Data.Calc.Number

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Fail

newtype FunctionBuilderMonadT m a =
    FunctionBuilderMonadT (ReaderT ([Expr Prim], ModeInfo) (MaybeT m) a)
                          deriving (Functor, Applicative, Monad)

instance MonadTrans FunctionBuilderMonadT where
    -- Whee! I feel so lifted!
    lift = FunctionBuilderMonadT . lift . lift

deriving instance Monad m => MonadReader ([Expr Prim], ModeInfo) (FunctionBuilderMonadT m)
deriving instance Monad m => MonadFail (FunctionBuilderMonadT m)
-- TODO Other derived monad instancse

askMode :: Monad m => FunctionBuilderMonadT m ModeInfo
askMode = asks snd

askArgs :: Monad m => FunctionBuilderMonadT m [Expr Prim]
askArgs = asks fst

asksMode :: Monad m => (ModeInfo -> b) -> FunctionBuilderMonadT m b
asksMode f = asks (f . snd)

asksArgs :: Monad m => ([Expr Prim] -> b) -> FunctionBuilderMonadT m b
asksArgs f = asks (f . fst)
