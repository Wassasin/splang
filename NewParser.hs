module NewParser where

import qualified Source
import qualified Lexer
import qualified AST
import Meta

type Token = Lexer.Token Source.IndexSpan

data Error = Unexpected Token | EndOfStream | Ambiguity
	deriving (Show, Eq)
type ParseInput = (Source.IndexSpan, [Token])

data ParseResult a = Match [(a, ParseInput)] | NoMatch Error
type ParseFunc a = ParseInput -> ParseResult a
data ParseFuncD a = PF (ParseFunc a)

bo :: ParseFuncD a -> ParseFunc a
bo (PF f) = f

mo :: ParseFunc a -> ParseFuncD a
mo f = PF f

parse :: ParseFuncD a -> [Token] -> Either [a] Error
parse f is = (case (bo f) (Source.IndexSpan 0 0, is) of
		NoMatch e	-> Right e
		Match xs	-> (case dualmap (\ i -> (case i of
					(x, (_, []))	-> Left x	-- Completely parsed, possible result
					(_, (_, t:_))	-> Right t	-- Tokens left to parse
				)) xs of
					([], ts)	-> Right (foldr1 bestNoMatch (map (\ t -> Unexpected t) ts)) -- If no options, show last token where parsing terminated
					(xs, _)		-> Left xs	-- Return all possible options (ideally one option)
			)
	)

dualmap :: (a -> Either b c) -> [a] -> ([b], [c])
dualmap f xs = dualmapr f xs ([], [])
	where
		dualmapr :: (a -> Either b c) -> [a] -> ([b], [c]) -> ([b], [c])
		dualmapr f [] (bs, cs)		= (bs, cs)
		dualmapr f (x:xs) (bs, cs)	= case f x of
			Left b	-> dualmapr f xs (b:bs, cs)
			Right c	-> dualmapr f xs (bs, c:cs)

bestNoMatch :: Error -> Error -> Error
bestNoMatch EndOfStream _ = EndOfStream
bestNoMatch _ EndOfStream = EndOfStream
bestNoMatch (Unexpected (Lexer.Token x (Source.IndexSpan xx xy))) (Unexpected (Lexer.Token y (Source.IndexSpan yx yy)))
	| xx <= yx	= Unexpected (Lexer.Token y (Source.IndexSpan yx yy))
	| otherwise	= Unexpected (Lexer.Token x (Source.IndexSpan xx xy))

(<|>) :: ParseFuncD a -> ParseFuncD a -> ParseFuncD a
(<|>) fd gd = mo (\ i -> case ((bo fd) i, (bo gd) i) of
		(Match xs, Match ys)	-> Match (xs ++ ys)
		(Match xs, NoMatch _)	-> Match xs
		(NoMatch _, Match ys)	-> Match ys
		(NoMatch t, NoMatch u)	-> NoMatch (bestNoMatch t u)
	)

instance Monad ParseFuncD where
--	(>>=) :: ParseFuncD a -> (a -> ParseFuncD b) -> ParseFuncD b
	(>>=) fd gm = mo (\ (l0, xs) -> case (bo fd) (l0, xs) of
			NoMatch e -> NoMatch e
			Match xms -> case dualmap (
					\(a, (l1, ys)) -> case bo (gm a) (l1, ys) of
						NoMatch e -> Left e
						Match yms -> Right (map (\ (b, (l2, zs)) -> (b, (foldr1 Source.merge [l0, l1, l2], zs))) yms)
				) xms of
					(errors, []) -> NoMatch (foldr1 bestNoMatch errors)
					(_, matchesSet) -> Match (concat matchesSet)
		)

--	return :: a -> ParseFuncD a
	return a = mo (\ i -> Match [(a, i)])

produce :: (Source.IndexSpan -> a) -> ParseFuncD a
produce f = mo (\ (l, xs) -> Match [(f l, (l, xs))])

produceP1 :: (P1Meta -> a) -> ParseFuncD a
produceP1 f = produce (f . constructP1)

newObject :: ParseFuncD a -> ParseFuncD a
newObject fd = mo (\ (Source.IndexSpan from to, is) -> case (bo fd) (Source.IndexSpan to to, is) of
		NoMatch e	-> NoMatch e
		Match xs	-> Match (map (\output -> case output of
				(x, (Source.IndexSpan _ rto, os)) -> (x, (Source.IndexSpan from rto, os))
			) xs)
	)

parseToken :: (Lexer.TokenE -> Bool) -> ParseFuncD Lexer.TokenE
parseToken f = mo (\ i -> case i of
		(_, [])				-> NoMatch EndOfStream
		(il, (Lexer.Token t l:xs))	-> case f t of
			True -> Match [(t, (Source.merge il l, xs))]
			False -> NoMatch (Unexpected (Lexer.Token t l))
	)
	
equalsToken :: Lexer.TokenE -> ParseFuncD Lexer.TokenE
equalsToken t = parseToken ((==) t)

parseOne :: (Token -> Maybe a) -> ParseFuncD a
parseOne f = mo (\ i -> case i of
		(_, [])				-> NoMatch EndOfStream
		(il, (Lexer.Token t l):xs)	-> case f (Lexer.Token t l) of
			Nothing -> NoMatch (Unexpected (Lexer.Token t l))
			Just result -> Match [(result, (Source.merge il l, xs))]
	)

parseBasicType :: ParseFuncD (P1 AST.Type)
parseBasicType = parseOne ( \x -> case x of
		Lexer.Token (Lexer.Type t) l -> Just (case t of
			Lexer.Void	-> AST.Void (constructP1 l)
			Lexer.Int	-> AST.Int (constructP1 l)
			Lexer.Bool	-> AST.Bool (constructP1 l))
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
			produceP1 (AST.Identifier i)
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

parseIdentifier :: ParseFuncD AST.Identifier
parseIdentifier = parseOne ( \x -> case x of
		(Lexer.Token (Lexer.Identifier str) l) -> Just str
		_ -> Nothing
	)

parseInteger :: ParseFuncD AST.Integer
parseInteger = parseOne ( \x -> case x of
		(Lexer.Token (Lexer.Integer n) _) -> Just n
		_ -> Nothing
	)
