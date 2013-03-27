module Parser (ParserLib.Error(..), parseSPL) where

import ParserLib

import qualified Source
import qualified Lexer
import qualified AST
import Meta

parseSPL :: [Lexer.Token] -> Either [P1 AST.Program] Error
parseSPL tokens = parse parseProgram tokens

parseProgram :: ParseFuncD (P1 AST.Program)
parseProgram = do
	decls <- many1 parseDecl
	produceP1 (AST.Program decls)

parseDecl :: ParseFuncD (P1 AST.Decl)
parseDecl = parseVarDecl <|> parseFunDecl

parseVarDecl :: ParseFuncD (P1 AST.Decl)
parseVarDecl = do
	t <- parseType
	i <- parseIdentifier
	equalsToken Lexer.AssignmentSign
	e <- parseExpr
	equalsToken Lexer.Semicolon
	produceP1 (AST.VarDecl t i e)

parseFunDecl :: ParseFuncD (P1 AST.Decl)
parseFunDecl = do
	t <- parseVoidType <!> parseType
	i <- parseIdentifier
	equalsToken Lexer.ParenthesesOpen
	fargs <- parseFargs
	equalsToken Lexer.ParenthesesClose
	equalsToken Lexer.CurlyBracketOpen
	vdecls <- many parseVarDecl
	stmts <- many1 parseStmt
	equalsToken Lexer.CurlyBracketClose
	produceP1 (AST.FunDecl t i fargs vdecls stmts)

parseFarg :: ParseFuncD (P1 AST.Type, P1 AST.Identifier)
parseFarg = do
	t <- newObject parseType
	i <- newObject parseIdentifier
	return (t, i)

parseFargs :: ParseFuncD [(P1 AST.Type, P1 AST.Identifier)]
parseFargs = manyd parseFarg (equalsToken Lexer.Comma)

parseStmt :: ParseFuncD (P1 AST.Stmt)
parseStmt = newObject (
	do
		equalsToken Lexer.CurlyBracketOpen
		stmts <- many parseStmt
		equalsToken Lexer.CurlyBracketClose
		produceP1 (AST.Scope stmts)
	<|> do
		equalsToken Lexer.If
		equalsToken Lexer.ParenthesesOpen
		expr <- parseExpr
		equalsToken Lexer.ParenthesesClose
		stmt <- parseStmt
		produceP1 (AST.If expr stmt)
	<|> do
		equalsToken Lexer.If
		equalsToken Lexer.ParenthesesOpen
		expr <- parseExpr
		equalsToken Lexer.ParenthesesClose
		tstmt <- parseStmt
		equalsToken Lexer.Else
		estmt <- parseStmt
		produceP1 (AST.IfElse expr tstmt estmt)
	<|> do
		equalsToken Lexer.While
		equalsToken Lexer.ParenthesesOpen
		expr <- parseExpr
		equalsToken Lexer.ParenthesesClose
		stmt <- parseStmt
		produceP1 (AST.While expr stmt)
	<|> do
		i <- parseIdentifier
		equalsToken Lexer.AssignmentSign
		expr <- parseExpr
		equalsToken Lexer.Semicolon
		produceP1 (AST.Assignment i expr)
	<|> do
		equalsToken Lexer.Return
		expr <- opt parseExpr
		equalsToken Lexer.Semicolon
		produceP1 (AST.Return expr)
	<|> do
		expr <- parseExpr
		equalsToken Lexer.Semicolon
		produceP1 (AST.Expr expr)
	)

parseExpr :: ParseFuncD (P1 AST.Expr)
parseExpr = newObject parseTerm1

parseTerm1 :: ParseFuncD (P1 AST.Expr)
parseTerm1 = do
		b <- parseOpNot;
		expr <- parseTerm3;
		produceP1 (AST.Unop b expr)	
	<!> do	expr1 <- parseTerm2
		(do
				b <- parseOp2Bool
				expr2 <- parseTerm1
				produceP1 (AST.Binop expr1 b expr2)
			<!>	return expr1)

parseTerm2 :: ParseFuncD (P1 AST.Expr)
parseTerm2 = do
		expr1 <- parseTerm3
		(do
				b <- parseOp2Equal
				expr2 <- parseTerm2
				produceP1 (AST.Binop expr1 b expr2)
			<!>	return expr1)

parseTerm3 :: ParseFuncD (P1 AST.Expr)
parseTerm3 = do
		e1 <- parseTerm4
		(do
				b <- parseOp2Cons
				e2 <- parseTerm3
				produceP1 (AST.Binop e1 b e2)
			<!>	return e1)

parseTerm4 :: ParseFuncD (P1 AST.Expr)
parseTerm4 = do
		e <- parseTerm5
		parseTerm4b e

parseTerm4b :: (P1 AST.Expr) -> ParseFuncD (P1 AST.Expr)
parseTerm4b e1 = (do
		b <- parseOp2Add
		e2 <- parseTerm5
		result <- produceP1 (AST.Binop e1 b e2)
		parseTerm4b result)
	<!> do
		return e1

parseTerm5 :: ParseFuncD (P1 AST.Expr)
parseTerm5 = do
		e <- parseTerm6
		parseTerm5b e

parseTerm5b e1 = (do
		b <- parseOp2Mult
		e2 <- parseTerm6
		result <- produceP1 (AST.Binop e1 b e2)
		parseTerm5b result)
	<!> do
		return e1

parseTerm6 = parseTerm7
	<!> do
		b <- parseOpNegative
		e <- parseTerm6
		produceP1 (AST.Unop b e)

parseTerm7 :: ParseFuncD (P1 AST.Expr)
parseTerm7 = parseKint
	<!> (do
		equalsToken Lexer.ParenthesesOpen
		e1 <- parseExpr
		do
				equalsToken Lexer.ParenthesesClose
				return e1
			<!> do
				equalsToken Lexer.Comma
				e2 <- parseExpr
				equalsToken Lexer.ParenthesesClose
				produceP1 (AST.Pair e1 e2))
	<!> do
		equalsToken Lexer.TrueT
		produceP1 (AST.Kbool True)
	<!> do
		equalsToken Lexer.FalseT
		produceP1 (AST.Kbool False)
	<!> (do
			parseVar
		<|> do
			i <- parseIdentifier
			equalsToken Lexer.ParenthesesOpen
			args <- parseActArgs
			equalsToken Lexer.ParenthesesClose
			produceP1 (AST.FunCall i args))
	<!> do
		equalsToken Lexer.SquareBracketsOpen
		equalsToken Lexer.SquareBracketsClose
		produceP1 AST.Nil

parseVar :: ParseFuncD (P1 AST.Expr)
parseVar = newObject ( do
		i <- parseIdentifier
		produceP1 (AST.Var i)
	)

parseKint :: ParseFuncD (P1 AST.Expr)
parseKint = newObject ( do
		n <- parseInteger
		produceP1 (AST.Kint n)
	)
	
parseActArgs :: ParseFuncD [P1 AST.Expr]
parseActArgs = manyd parseExpr (equalsToken Lexer.Comma)

parseVoidType :: ParseFuncD (P1 AST.Type)
parseVoidType = parseOne ( \x -> case x of
		Lexer.Token (Lexer.Type t) l -> case t of
			Lexer.Void -> Just $ AST.Void (constructP1 l)
			_ -> Nothing
		_ -> Nothing
	)

parseBasicType :: ParseFuncD (P1 AST.Type)
parseBasicType = parseOne ( \x -> case x of
		Lexer.Token (Lexer.Type t) l -> case t of
			Lexer.Int	-> Just $ AST.Int (constructP1 l)
			Lexer.Bool	-> Just $ AST.Bool (constructP1 l)
			_ -> Nothing
		_ -> Nothing
	)

parseType :: ParseFuncD (P1 AST.Type)
parseType = newObject (
			parseBasicType
		<|>	do
			equalsToken Lexer.ParenthesesOpen
			t1 <- parseType
			equalsToken Lexer.Comma
			t2 <- parseType
			equalsToken Lexer.ParenthesesClose
			produceP1 (AST.Product t1 t2)
		<|>	do
			equalsToken Lexer.SquareBracketsOpen
			t <- parseType
			equalsToken Lexer.SquareBracketsClose
			produceP1 (AST.ListType t)
		<|>	do
			i <- parseIdentifier
			produceP1 (AST.TypeIdentifier i)
	)

operatorToken :: (Lexer.OperatorE -> Maybe (P1Meta -> a)) -> ParseFuncD a
operatorToken f = parseOne ( \t -> case t of
		Lexer.Token (Lexer.Operator op) l -> case f op of
			Just x	-> Just (x (constructP1 l))
			Nothing	-> Nothing
		_ -> Nothing
	)

parseOp2Mult :: ParseFuncD (P1 AST.BinaryOperator)
parseOp2Mult = operatorToken (\op -> case op of
		Lexer.Multiplication	-> Just AST.Multiplication
		Lexer.Division		-> Just AST.Division
		Lexer.Modulo		-> Just AST.Modulo
		_			-> Nothing
	)

parseOp2Add :: ParseFuncD (P1 AST.BinaryOperator)
parseOp2Add = operatorToken (\op -> case op of
		Lexer.Plus	-> Just AST.Plus
		Lexer.Minus	-> Just AST.Minus
		_		-> Nothing
	)

parseOp2Cons :: ParseFuncD (P1 AST.BinaryOperator)
parseOp2Cons = operatorToken (\op -> case op of
		Lexer.Cons	-> Just AST.Cons
		_		-> Nothing
	)

parseOp2Equal :: ParseFuncD (P1 AST.BinaryOperator)
parseOp2Equal = operatorToken (\op -> case op of
		Lexer.Equals		-> Just AST.Equals
		Lexer.LesserThan	-> Just AST.LesserThan
		Lexer.GreaterThan	-> Just AST.GreaterThan
		Lexer.LesserEqualThan	-> Just AST.LesserEqualThan
		Lexer.GreaterEqualThan	-> Just AST.GreaterEqualThan
		Lexer.Nequals		-> Just AST.Nequals
		_ -> Nothing
	)

parseOp2Bool :: ParseFuncD (P1 AST.BinaryOperator)
parseOp2Bool = operatorToken (\op -> case op of
		Lexer.And		-> Just AST.And
		Lexer.Or		-> Just AST.Or
		_ -> Nothing
	)

parseOpNot :: ParseFuncD (P1 AST.UnaryOperator)
parseOpNot = operatorToken (\op -> case op of
		Lexer.Not		-> Just AST.Not
		_ -> Nothing
	)

parseOpNegative :: ParseFuncD (P1 AST.UnaryOperator)
parseOpNegative = operatorToken (\op -> case op of
		Lexer.Minus -> Just AST.Negative
		_ -> Nothing
	)

parseIdentifier :: ParseFuncD (P1 AST.Identifier)
parseIdentifier = parseOne ( \x -> case x of
		(Lexer.Token (Lexer.Identifier str) l) -> Just (AST.Identifier str Nothing (constructP1 l))
		_ -> Nothing
	)

parseInteger :: ParseFuncD AST.Integer
parseInteger = parseOne ( \x -> case x of
		(Lexer.Token (Lexer.Integer n) _) -> Just n
		_ -> Nothing
	)
