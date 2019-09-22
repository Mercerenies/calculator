{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Data.Calc.Pass(PassT(..), Pass, pass,
                      (.), id, -- Re-export the Control.Category versions
                      runPassOnceTDM, runPassOnceBUM, runPassOnceFullM,
                      runPassOnceTD, runPassOnceBU, runPassOnceFull,
                      runPassTDM, runPassBUM, runPassFullM,
                      runPassTD, runPassBU, runPassFull,
                      fromKleisli, toKleisli, conditionalPass,
                      liftPassT) where

import Data.Calc.Expr
import Data.Calc.Util

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Profunctor

newtype PassT m a b = PassT (Expr a -> m (Expr b))
    deriving (Functor)

type Pass = PassT Identity

instance Monad m => Category (PassT m) where
    id = PassT pure
    PassT f . PassT g = PassT (f <=< g)

instance Functor m => Profunctor (PassT m) where
    dimap f g (PassT h) = PassT (fmap (fmap g) . h . fmap f)

pass :: Applicative m => (Expr a -> Expr b) -> PassT m a b
pass f = PassT (pure . f)

runPassOnceTDM :: Monad m => PassT m a a -> Expr a -> m (Expr a)
runPassOnceTDM (PassT f) e =
    f e >>= \case
      Constant x -> pure $ Constant x
      Compound h ts -> Compound h <$> mapM (runPassOnceTDM (PassT f)) ts

runPassOnceBUM :: Monad m => PassT m a a -> Expr a -> m (Expr a)
runPassOnceBUM (PassT f) e = e' >>= f
    where e' = case e of
                 Constant x -> pure $ Constant x
                 Compound h ts -> Compound h <$> mapM (runPassOnceBUM (PassT f)) ts

runPassOnceFullM :: Monad m => PassT m a a -> Expr a -> m (Expr a)
runPassOnceFullM (PassT f) e = e' >>= f
    where e' = f e >>= \case
                 Constant x -> pure (Constant x)
                 Compound h ts -> Compound h <$> mapM (runPassOnceFullM (PassT f)) ts

runPassOnceTD :: Pass a a -> Expr a -> Expr a
runPassOnceTD p = runIdentity . runPassOnceTDM p

runPassOnceBU :: Pass a a -> Expr a -> Expr a
runPassOnceBU p = runIdentity . runPassOnceBUM p

runPassOnceFull :: Pass a a -> Expr a -> Expr a
runPassOnceFull p = runIdentity . runPassOnceFullM p

runPassTDM :: (Eq a, Monad m) => PassT m a a -> Expr a -> m (Expr a)
runPassTDM p = untilFixedM (runPassOnceTDM p)

runPassBUM :: (Eq a, Monad m) => PassT m a a -> Expr a -> m (Expr a)
runPassBUM p = untilFixedM (runPassOnceBUM p)

runPassFullM :: (Eq a, Monad m) => PassT m a a -> Expr a -> m (Expr a)
runPassFullM p = untilFixedM (runPassOnceFullM p)

runPassTD :: Eq a => Pass a a -> Expr a -> Expr a
runPassTD p = runIdentity . runPassTDM p

runPassBU :: Eq a => Pass a a -> Expr a -> Expr a
runPassBU p = runIdentity . runPassBUM p

runPassFull :: Eq a => Pass a a -> Expr a -> Expr a
runPassFull p = runIdentity . runPassFullM p

fromKleisli :: Monad m => Kleisli m (Expr a) (Expr b) -> PassT m a b
fromKleisli (Kleisli f) = PassT f

toKleisli :: Monad m => PassT m a b -> Kleisli m (Expr a) (Expr b)
toKleisli (PassT f) = (Kleisli f)

conditionalPass :: Monad m => (Expr a -> m Bool) -> PassT m a a -> PassT m a a
conditionalPass p f = fromKleisli $ possibly (Kleisli p) (toKleisli f)

liftPassT :: (MonadTrans t, Monad m) => PassT m a b -> PassT (t m) a b
liftPassT (PassT p) = PassT $ lift . p
