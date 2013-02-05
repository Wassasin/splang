module Lexer where

data Location = Location Int Int
	deriving (Show, Eq)

data TypeE = Void
	| Int
	| Bool
	deriving (Show, Eq)

data OperatorE = Plus
	| Minus
	| Multiplication
	| Division
	| Modulo
	| Equals
	| LesserThan
	| GreaterThan
	| LesserEqualThan
	| GreaterEqualThan
	| Nequals
	| And
	| Or
	| Cons
	| Not
	deriving (Show, Eq)

data TokenE = Comment String
	| CurlyBracketOpen
	| CurlyBracketClose
	| AssignmentSign
	| Semicolon
	| ParenthesesOpen
	| ParenthesesClose
	| Type TypeE
	| SquareBracketsOpen
	| SquareBracketsClose
	| Comma
	| Identifier String
	| If
	| Else
	| While
	| Return
	| Integer Int
	| TrueT
	| FalseT
	| Operator OperatorE
	deriving (Show, Eq)

data Token = Token TokenE Location
	deriving (Show, Eq)

data LexerResult = Match [Token] (String, Int)
	| NoMatch Int

literalMap :: [(String, TokenE)]
literalMap = [
		("{", CurlyBracketOpen),
		("}", CurlyBracketClose),
		("=", AssignmentSign),
		(";", Semicolon),
		("(", ParenthesesOpen),
		(")", ParenthesesClose),
		("Void", Type Void),
		("Int", Type Int),
		("Bool", Type Bool),
		("[", SquareBracketsOpen),
		("]", SquareBracketsClose),
		(",", Comma),
		("if", If),
		("else", Else),
		("while", While),
		("return", Return),
		("true", TrueT),
		("false", FalseT),
		("+", Operator Plus),
		("-", Operator Minus),
		("*", Operator Multiplication),
		("/", Operator Division),
		("%", Operator Modulo),
		("==", Operator Equals),
		("<", Operator LesserThan),
		(">", Operator GreaterThan),
		("<=", Operator LesserEqualThan),
		(">=", Operator GreaterEqualThan),
		("!=", Operator Nequals),
		("&&", Operator And),
		("||", Operator Or),
		(":", Operator Cons),
		("!", Operator Not)
	]

isPrefixOf :: String -> String -> Bool
isPrefixOf (x:xs) (y:ys)	= x == y && (isPrefixOf xs ys)
isPrefixOf (x:xs) []		= False
isPrefixOf _ _			= True

findstr :: String -> String -> Maybe Int
findstr _ []			= Nothing
findstr needle str
	| isPrefixOf needle str	= Just 0
	| otherwise		= case findstr needle (tail str) of
					Just n -> Just (n+1)
					Nothing -> Nothing

substr :: String -> Int -> Int -> String
substr _ _ 0		= []
substr (x:xs) 0 n	= (x : substr xs 0 (n-1))
substr (x:xs) i n	= substr xs (i-1) n

precut :: String -> Int -> String
precut str 0	= str
precut (x:xs) i	= precut xs (i-1)

consumeWhitespace :: (String, Int) -> LexerResult
consumeWhitespace (' ':xs, i) = Match [] (xs, i+1)
consumeWhitespace ('\t':xs, i) = Match [] (xs, i+1)
consumeWhitespace ('\n':xs, i) = Match [] (xs, i+1)
consumeWhitespace (xs, i) = NoMatch i

consumeComment :: (String, Int) -> LexerResult
consumeComment (str, start)
	| not (isPrefixOf "/*" str)	= NoMatch start
	| otherwise			= case findstr "*/" (precut str 2) of
						Nothing -> NoMatch (start+2)
						Just n -> let
								length = 2+n+2
								end = start+length
								in Match [Token (Comment (substr str 2 n)) (Location start end)] (precut str length, end)
