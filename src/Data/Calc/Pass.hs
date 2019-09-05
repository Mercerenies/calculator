{-# LANGUAGE DeriveFunctor #-}

module Data.Calc.Pass(Pass(..),
                      (.), id, -- Re-export the Control.Category versions
                      runPassOnceTD, runPassOnceBU, runPassOnceFull,
                      runPassTD, runPassBU, runPassFull) where

import Data.Calc.Expr
import Data.Calc.Util

import Prelude hiding (id, (.))
import Control.Category
import Data.Profunctor

newtype Pass a b = Pass (Expr a -> Expr b)
    deriving (Functor)

instance Category Pass where
    id = Pass (id)
    Pass f . Pass g = Pass (f . g)

instance Profunctor Pass where
    dimap f g (Pass h) = Pass (fmap g . h . fmap f)

runPassOnceTD :: Pass a a -> Expr a -> Expr a
runPassOnceTD (Pass f) e =
    case f e of
      Constant x -> Constant x
      Compound h ts -> Compound h $ fmap (runPassOnceTD (Pass f)) ts

runPassOnceBU :: Pass a a -> Expr a -> Expr a
runPassOnceBU (Pass f) e = f e'
    where e' = case e of
                 Constant x -> Constant x
                 Compound h ts -> Compound h $ fmap (runPassOnceBU (Pass f)) ts

runPassOnceFull :: Pass a a -> Expr a -> Expr a
runPassOnceFull (Pass f) e = f e'
    where e' = case f e of
                 Constant x -> Constant x
                 Compound h ts -> Compound h $ fmap (runPassOnceFull (Pass f)) ts

runPassTD :: Eq a => Pass a a -> Expr a -> Expr a
runPassTD p = untilFixed (runPassOnceTD p)

runPassBU :: Eq a => Pass a a -> Expr a -> Expr a
runPassBU p = untilFixed (runPassOnceBU p)

runPassFull :: Eq a => Pass a a -> Expr a -> Expr a
runPassFull p = untilFixed (runPassOnceFull p)
