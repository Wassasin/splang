module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Pos
import qualified Lexer

type MyParser a   = GenParser Lexer.Token () a

data Identifier = Identifier String
	deriving (Show, Eq)
data SLInteger = SLInteger Int
	deriving (Show, Eq)
data Op2Mult = Multiplication | Division | Modulo
	deriving (Show, Eq)

data Term4 = BOPid Identifier Op2Mult Term4
	| BOPint SLInteger Op2Mult Term4
	| Kid Identifier
	| Kint SLInteger
	deriving (Show, Eq)

mytoken :: (Lexer.Token -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken tok   = show tok
    posToken  (Lexer.Token _ (Lexer.Location c r))   = newPos "bla" c r
    testToken tok   = test tok

parseIdentifier :: MyParser Identifier
parseIdentifier = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Identifier str) _) -> Just (Identifier str)
	_ -> Nothing )

parseSLInteger :: MyParser SLInteger
parseSLInteger = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Integer n) _) -> Just (SLInteger n)
	_ -> Nothing )

parseOp2Mult :: MyParser Op2Mult
parseOp2Mult = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Operator op) _) -> case op of
		Lexer.Multiplication -> Just Multiplication
		Lexer.Division -> Just Division
		Lexer.Modulo -> Just Modulo
		_ -> Nothing
	_ -> Nothing )

parseTerm4 :: MyParser Term4
parseTerm4 = do { i <- parseIdentifier;
				do { b <- parseOp2Mult; t <- parseTerm4; return (BOPid i b t) }
				<|> return (Kid i) }
	<|> do { n <- parseSLInteger;
				do { b <- parseOp2Mult; t <- parseTerm4; return (BOPint n b t) }
				<|> return (Kint n) }
