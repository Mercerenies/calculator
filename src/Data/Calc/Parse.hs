
module Data.Calc.Parse(assocToOpAssoc, operatorTable, parseExpr) where

import Data.Calc.Expr
import Data.Calc.Operator
import Data.Calc.Number
import Data.Calc.Util

import Text.Parsec
import qualified Text.Parsec.Expr as Expr
import Text.ParserCombinators.Parsec.Number(decimalFloat, floating2, sign)
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function
import Data.Ord
import Data.Ratio
import Data.Complex
import Control.Monad

type Parser = Parsec String ()

nat :: Parser Integer
nat = read <$> many1 digit

double :: Parser Number
double = either (NRatio . fromInteger) NDouble <$> decimalFloat

rat :: Parser Number
rat = try $ do
        num <- nat
        _ <- char ':'
        denom <- nat
        guard $ denom /= 0
        return $ NRatio (num % denom)

complexNumber :: Parser Number
complexNumber = do
  _ <- char '(' <* spaces
  re <- sign <*> floating2 True
  _ <- spaces *> char ',' <* spaces
  im <- sign <*> floating2 True
  _ <- spaces *> char ')'
  return . NComplex $ re :+ im

var :: Parser String
var = (:) <$> varStartChar <*> many varChar
    where varStartChar = letter
          varChar = varStartChar <|> digit

arglist :: Parser [Expr Prim]
arglist = sepBy expr (try $ spaces *> char ',' *> spaces)

fnCall :: Parser (Expr Prim)
fnCall = do
  -- Note no space between the name and the paren!
  name <- try (var <* char '(')
  spaces
  args <- arglist
  spaces
  _ <- char ')'
  return $ Compound name args

atom :: Parser (Expr Prim)
atom = ((try $ Constant . PrimNum <$> complexNumber) <?> "complex number") <|>
       (char '(' *> spaces *> expr <* spaces <* char ')') <|>
       (Constant . PrimNum <$> rat <?> "rational number") <|>
       (Constant . PrimNum <$> double <?> "real number") <|>
       (fnCall <?> "function") <|>
       (Constant . PrimVar <$> var <?> "variable")

assocToOpAssoc :: Assoc -> Expr.Assoc
assocToOpAssoc NoAssoc = Expr.AssocNone
assocToOpAssoc LeftAssoc = Expr.AssocLeft
assocToOpAssoc RightAssoc = Expr.AssocRight

operatorTable :: Expr.OperatorTable String () Identity (Expr Prim)
operatorTable = Map.toList stdOps &
                List.sortBy (compare `on` (Down . opPrec)) &
                List.groupBy ((==) `on` opPrec) &
                map (map opToOperator)
    where opPrec (_, Op _ prec _ _) = prec
          parseBinOp :: String -> String -> Parser (Expr b -> Expr b -> Expr b)
          parseBinOp s name = (\x y -> Compound s [x, y]) <$ try (spaces *> string name <* spaces)
          parseUnOp :: String -> String -> Parser (Expr b -> Expr b)
          parseUnOp s name = (\x -> Compound s [x]) <$ try (spaces *> string name <* spaces)
          opToOperator :: (String, Op) -> Expr.Operator String () Identity (Expr Prim)
          opToOperator (s, Op name _ assoc Infix) =
              Expr.Infix (parseBinOp s (stripString name)) (assocToOpAssoc assoc)
          opToOperator (s, Op name _ _ Postfix) =
              Expr.Postfix (parseUnOp s (stripString name))
          opToOperator (s, Op name _ _ Prefix) =
              Expr.Prefix (parseUnOp s (stripString name))

expr :: Parser (Expr Prim)
expr = Expr.buildExpressionParser operatorTable atom

parseExpr :: SourceName -> String -> Either ParseError (Expr Prim)
parseExpr = parse (expr <* eof)
