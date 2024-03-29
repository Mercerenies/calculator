
module Data.Calc.Print(lispLikeShows, lispLikeShow,
                       formulaShowsPrec, formulaShows, formulaShow) where

import Data.Calc.Expr
import Data.Calc.Operator

import Data.List(intersperse)

lispLikeShows :: Show a => Expr a -> ShowS
lispLikeShows (Constant x) = shows x
lispLikeShows (Compound h ts) = ("(" ++) . (h ++) . rest . (")" ++)
    where rest = foldr (.) id $ map (\t -> (" " ++) . lispLikeShows t) ts

lispLikeShow :: Show a => Expr a -> String
lispLikeShow e = lispLikeShows e ""

formulaShowsPrec :: Show a => Int -> Expr a -> ShowS
formulaShowsPrec _ (Constant x) = shows x
formulaShowsPrec n x
    | Compound "vector" ts <- x = ("[" ++) . showsArglist ts . ("]" ++)
    | Compound h ts <- x =
                 case getStdOp h of
                   Nothing -> showsAsFunction h ts
                   Just (Op name prec _ fix) -> showParen (n >= prec) $ internal fix h name ts prec
    where showsAsFunction h' ts' = (h' ++) . ("(" ++) . showsArglist ts' . (")" ++)
          showsArglist xs =
              foldr (.) id . intersperse (", " ++) . map (formulaShowsPrec 0) $ xs
          internal Infix _ name args prec | length args >= 2 =
              foldr (.) id . intersperse (name ++) .
                    map (formulaShowsPrec prec) $ args
          internal Prefix _ name [a] prec =
              (name ++) . formulaShowsPrec prec a
          internal Postfix _ name [a] prec =
              formulaShowsPrec prec a . (name ++)
          internal _ h _ args _ = showsAsFunction h args

formulaShows :: Show a => Expr a -> ShowS
formulaShows = formulaShowsPrec 0

formulaShow :: Show a => Expr a -> String
formulaShow e = formulaShows e ""
