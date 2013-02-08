module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Pos

import qualified Lexer
import qualified Source
import qualified AST

type MyParser a   = GenParser Lexer.Token () a

mytoken :: (Lexer.Token -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken tok   = show tok
    posToken  (Lexer.Token _ (Source.IndexSpan c r))   = newPos "bla" c r
    testToken tok   = test tok

equalsToken :: Lexer.Token -> MyParser ()
equalsToken tok = mytoken ( \x -> case x of
	(Lexer.Token tok _) -> Just ()
	_ -> Nothing)

parseIdentifier :: MyParser AST.Identifier
parseIdentifier = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Identifier str) _) -> Just str
	_ -> Nothing )

parseInteger :: MyParser AST.Integer
parseInteger = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Integer n) _) -> Just n
	_ -> Nothing )

parseOp2Mult :: MyParser AST.BinaryOperator
parseOp2Mult = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Operator op) _) -> case op of
		Lexer.Multiplication -> Just AST.Multiplication
		Lexer.Division -> Just AST.Division
		Lexer.Modulo -> Just AST.Modulo
		_ -> Nothing
	_ -> Nothing )

parseTerm4 :: MyParser AST.Expr
parseTerm4 = do { i <- parseIdentifier;
				do { b <- parseOp2Mult; t <- parseTerm4; return (AST.Binop (AST.Var i) b t) }
				<|> return (AST.Var i) }
	<|> do { n <- parseInteger;
				do { b <- parseOp2Mult; t <- parseTerm4; return (AST.Binop (AST.Kint n) b t) }
				<|> return (AST.Kint n) }
