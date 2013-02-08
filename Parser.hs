module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Pos

import qualified Lexer
import qualified Source

type MyParser a   = GenParser Lexer.Token () a

data Identifier = Identifier String
	deriving (Show, Eq)
data SLInteger = SLInteger Int
	deriving (Show, Eq)
data BinaryOperator = Multiplication | Division | Modulo
	| Plus | Minus | Cons
	| Equal | LesserThan | GreaterThan | LesserEqualThen | GreaterEqualThan | Nequal
	| And | Or
	deriving (Show, Eq)

data UnaryOperator = Not | Negative
	deriving (Show, Eq)

data Expr = Binop Expr BinaryOperator Expr
	| Unop UnaryOperator Expr
	| Var Identifier
	| Kint SLInteger
	deriving (Show, Eq)

mytoken :: (Lexer.Token -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken tok   = show tok
    posToken  (Lexer.Token _ (Source.IndexSpan c r))   = newPos "bla" c r
    testToken tok   = test tok

parseIdentifier :: MyParser Identifier
parseIdentifier = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Identifier str) _) -> Just (Identifier str)
	_ -> Nothing )

parseSLInteger :: MyParser SLInteger
parseSLInteger = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Integer n) _) -> Just (SLInteger n)
	_ -> Nothing )

parseOp2Mult :: MyParser BinaryOperator
parseOp2Mult = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Operator op) _) -> case op of
		Lexer.Multiplication -> Just Multiplication
		Lexer.Division -> Just Division
		Lexer.Modulo -> Just Modulo
		_ -> Nothing
	_ -> Nothing )

parseTerm4 :: MyParser Expr
parseTerm4 = do { i <- parseIdentifier;
				do { b <- parseOp2Mult; t <- parseTerm4; return (Binop (Var i) b t) }
				<|> return (Var i) }
	<|> do { n <- parseSLInteger;
				do { b <- parseOp2Mult; t <- parseTerm4; return (Binop (Kint n) b t) }
				<|> return (Kint n) }
