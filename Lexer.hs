module Lexer where

import Data.List (sortBy)
import Data.Char (isDigit, isAlpha, isAlphaNum)

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

type LexerFunc = (String, Int) -> LexerResult

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

findLast :: (Char -> Bool) -> String -> Maybe Int
findLast _ []		= Nothing
findLast f (x:xs)
	| not (f x)	= Nothing
	| otherwise	= case (findLast f xs) of
		Nothing	-> Just 0
		Just n	-> Just (n+1)

findstr :: String -> String -> Maybe Int
findstr _ []			= Nothing
findstr needle str
	| isPrefixOf needle str	= Just 0
	| otherwise		= case findstr needle (tail str) of
					Nothing -> Nothing
					Just n -> Just (n+1)

substr :: String -> Int -> Int -> String
substr _ _ 0		= []
substr (x:xs) 0 n	= (x : substr xs 0 (n-1))
substr (x:xs) i n	= substr xs (i-1) n

precut :: String -> Int -> String
precut str 0	= str
precut (x:xs) i	= precut xs (i-1)

(>>>) :: LexerFunc -> LexerFunc -> LexerFunc
(>>>) f g = \x -> case f x of
	Match tok loc	-> Match tok loc
	NoMatch i	-> g x

consumeWhitespace :: LexerFunc
consumeWhitespace (' ':xs, i) = Match [] (xs, i+1)
consumeWhitespace ('\t':xs, i) = Match [] (xs, i+1)
consumeWhitespace ('\n':xs, i) = Match [] (xs, i+1)
consumeWhitespace (xs, i) = NoMatch i

consumeComment :: LexerFunc
consumeComment (str, start)
	| not (isPrefixOf "/*" str)	= NoMatch start
	| otherwise			= case findstr "*/" (precut str 2) of
						Nothing -> NoMatch (start+2)
						Just n -> let
							size = 2+n+2
							end = start+size
							in Match [Token (Comment (substr str 2 n)) (Location start end)] (precut str size, end)

consumeLiteral :: LexerFunc
consumeLiteral = foldr1 (>>>) literalFuncs
	where literalFuncs = map f (reverse (sortBy (\(x, _) (y, _) -> compare (length x) (length y)) literalMap))
		where f (lstr, ltok) (str, start)
			| not (isPrefixOf lstr str) = NoMatch start
			| otherwise = let
				size = length lstr
				end = start + size
				in Match [Token ltok (Location start end)] (precut str size, end)

consumeInteger :: LexerFunc
consumeInteger (str, start)	= case findLast isDigit str of
					Nothing -> NoMatch start
					Just n -> let
						size = n+1
						end = start+size
						in Match [Token (Integer (read (substr str 0 size))) (Location start end)] (precut str size, end)

consumeIdentifier :: LexerFunc
consumeIdentifier (x:xs, start)
	| isAlpha x = f (x:xs, start) -- Also include first character for token Location
	| otherwise = NoMatch start
	where f (str, start)	= case findLast (\c -> isAlphaNum c || c == '_') str of
					Nothing -> NoMatch start
					Just n -> let
						size = n+1
						end = start+size
						in Match [Token (Identifier (substr str 0 size)) (Location start end)] (precut str size, end)

lextok :: LexerFunc
lextok = consumeWhitespace
	>>> consumeComment
	>>> consumeInteger
	>>> consumeLiteral
	>>> consumeIdentifier

lexer :: LexerFunc
lexer ([], n)	= Match [] ([], n)
lexer x		= case lextok x of
			NoMatch i -> NoMatch i
			Match ts y -> case lexer y of
				NoMatch i -> NoMatch i
				Match us ([], n) -> Match (ts ++ us) ([], n)
