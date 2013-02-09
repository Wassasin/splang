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

equalsToken :: Lexer.TokenE -> MyParser ()
equalsToken tok = mytoken ( \x -> case x of
	(Lexer.Token tok' _) -> if (tok == tok') then Just () else Nothing )


parseProgram :: MyParser AST.Program
parseProgram = many1 parseDecl

-- TODO: VarDecl and FunDecl start with the same prefix...
parseDecl :: MyParser AST.Decl
parseDecl = parseFunDecl <|> parseVarDecl

parseVarDecl :: MyParser AST.Decl
parseVarDecl = do
	t <- parseType
	i <- parseIdentifier
	_ <- equalsToken Lexer.AssignmentSign
	e <- parseExpr
	_ <- equalsToken Lexer.Semicolon
	return (AST.VarDecl t i e)

parseFunDecl :: MyParser AST.Decl
parseFunDecl = do
	t <- parseType
	i <- parseIdentifier
	_ <- equalsToken Lexer.ParenthesesOpen
	fargs <- parseFargs
	_ <- equalsToken Lexer.ParenthesesClose
	_ <- equalsToken Lexer.CurlyBracketOpen
	vdecls <- many parseVarDecl
	stmts <- many1 parseStmt
	_ <- equalsToken Lexer.CurlyBracketClose
	return (AST.FunDecl t i fargs vdecls stmts)

parseType :: MyParser AST.Type
parseType = mytoken (\x -> case x of
	Lexer.Token (Lexer.Type y) _ -> Just (case y of
		Lexer.Void -> AST.Void
		Lexer.Int -> AST.Int
		Lexer.Bool -> AST.Bool)
	_ -> Nothing)
	<|> do {
		_ <- equalsToken Lexer.ParenthesesOpen;
		t1 <- parseType;
		t2 <- parseType;
		_ <- equalsToken Lexer.ParenthesesClose;
		return (AST.Product t1 t2)}
	<|>	do {
		_ <- equalsToken Lexer.SquareBracketsOpen;
		t <- parseType;
		_ <- equalsToken Lexer.SquareBracketsClose;
		return (AST.ListType t)}
	<|> (parseIdentifier >>= return . AST.Identifier)

parseFarg :: MyParser (AST.Type, AST.Identifier)
parseFarg = do
	t <- parseType
	i <- parseIdentifier
	return (t, i)

parseFargs :: MyParser [(AST.Type, AST.Identifier)]
parseFargs = do {
	farg <- parseFarg;
	do {_ <- equalsToken Lexer.Comma; rest <- parseFargs; return (farg : rest)} <|> return [farg]
} <|> return []

-- TODO: Add if-else
parseStmt :: MyParser AST.Stmt
parseStmt = (equalsToken Lexer.CurlyBracketOpen >> (many1 parseStmt) >>= (\stmts -> equalsToken Lexer.CurlyBracketClose >> return (AST.Scope stmts)))
	<|> (equalsToken Lexer.If >> equalsToken Lexer.ParenthesesOpen >> parseExpr >>= (\expr -> equalsToken Lexer.ParenthesesClose >> parseStmt >>= (\stmt -> return (AST.If expr stmt))))
	<|> (equalsToken Lexer.While >> equalsToken Lexer.ParenthesesOpen >> parseExpr >>= (\expr -> equalsToken Lexer.ParenthesesClose >> parseStmt >>= (\stmt -> return (AST.While expr stmt))))
	<|> (parseIdentifier >>= (\id -> equalsToken Lexer.AssignmentSign >> parseExpr >>= (\expr -> equalsToken Lexer.Semicolon >> return (AST.Assignment id expr))))
	<|> (equalsToken Lexer.Return >> parseExpr >>= (\expr -> return (AST.Return expr)))

-- TODO: add FunCall
parseExpr :: MyParser AST.Expr
parseExpr = parseTerm1

parseTerm1 :: MyParser AST.Expr
parseTerm1 = do { expr1 <- parseTerm2;
                  do { b <- parseOp2Bool; expr2 <- parseTerm1; return (AST.Binop expr1 b expr2) }
                  <|> return expr1 }
         <|> do { b <- parseOpNot;
                  expr <- parseTerm3;
                  return (AST.Unop b expr) }

parseTerm2 :: MyParser AST.Expr
parseTerm2 = do { expr1 <- parseTerm3;
                  do { b <- parseOp2Equal; expr2 <- parseTerm2; return (AST.Binop expr1 b expr2) }
                  <|> return expr1 }

parseTerm3 :: MyParser AST.Expr
parseTerm3 = do { expr1 <- parseTerm4;
                  do { b <- parseOp2Add; expr2 <- parseTerm3; return (AST.Binop expr1 b expr2) }
                  <|> return expr1 }
         <|> do { b <- parseOpNegative;
                  expr <- parseTerm3;
                  return (AST.Unop b expr) }

parseTerm4 :: MyParser AST.Expr
parseTerm4 = do { i <- parseIdentifier;
				  do { b <- parseOp2Mult; t <- parseTerm4; return (AST.Binop (AST.Var i) b t) }
				  <|> return (AST.Var i) }
         <|> do { n <- parseInteger;
				  do { b <- parseOp2Mult; t <- parseTerm4; return (AST.Binop (AST.Kint n) b t) }
				  <|> return (AST.Kint n) }
         <|> do { _ <- equalsToken Lexer.ParenthesesOpen;
                  expr <- parseExpr;
                  _ <- equalsToken Lexer.ParenthesesClose;
                  return expr}
         <|> ((equalsToken Lexer.TrueT) >> return (AST.Kbool True))
         <|> ((equalsToken Lexer.FalseT) >> return (AST.Kbool False))
         <|> ((equalsToken Lexer.SquareBracketsOpen) >> (equalsToken Lexer.SquareBracketsOpen) >> return (AST.List []))

operatorToken :: (Lexer.OperatorE -> Maybe a) -> MyParser a
operatorToken f = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Operator op) _) -> f op
	_ -> Nothing )

parseOp2Mult :: MyParser AST.BinaryOperator
parseOp2Mult = operatorToken (\op -> case op of
		Lexer.Multiplication	-> Just AST.Multiplication
		Lexer.Division			-> Just AST.Division
		Lexer.Modulo			-> Just AST.Modulo
		_ -> Nothing)

parseOp2Add :: MyParser AST.BinaryOperator
parseOp2Add = operatorToken (\op -> case op of
		Lexer.Plus	-> Just AST.Plus
		Lexer.Minus	-> Just AST.Minus
		Lexer.Cons	-> Just AST.Cons
		_ -> Nothing)

parseOp2Equal :: MyParser AST.BinaryOperator
parseOp2Equal = operatorToken (\op -> case op of
		Lexer.Equals			-> Just AST.Equals
		Lexer.LesserThan		-> Just AST.LesserThan
		Lexer.GreaterThan		-> Just AST.GreaterThan
		Lexer.LesserEqualThan	-> Just AST.LesserEqualThan
		Lexer.GreaterEqualThan	-> Just AST.GreaterEqualThan
		Lexer.Nequals			-> Just AST.Nequals
		_ -> Nothing)

parseOp2Bool :: MyParser AST.BinaryOperator
parseOp2Bool = operatorToken (\op -> case op of
		Lexer.And		-> Just AST.And
		Lexer.Or		-> Just AST.Or
		_ -> Nothing)

parseOpNot :: MyParser AST.UnaryOperator
parseOpNot = operatorToken (\op -> case op of
		Lexer.Not		-> Just AST.Not
		_ -> Nothing)

parseOpNegative :: MyParser AST.UnaryOperator
parseOpNegative = operatorToken (\op -> case op of
		Lexer.Minus -> Just AST.Negative
		_ -> Nothing)

parseIdentifier :: MyParser AST.Identifier
parseIdentifier = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Identifier str) _) -> Just str
	_ -> Nothing )

parseInteger :: MyParser AST.Integer
parseInteger = mytoken ( \x -> case x of
	(Lexer.Token (Lexer.Integer n) _) -> Just n
	_ -> Nothing )
