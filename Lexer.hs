module Lexer where

import qualified Source

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

data Token loc = Token TokenE loc
	deriving (Show, Eq)

data LexerResult = Match [Token Source.IndexSpan] (String, Source.Index)
	| NoMatch Source.Index

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
		("True", TrueT),
		("False", FalseT),
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
	| (Source.isPrefixOf "/*" str) = case Source.findstr "*/" (Source.precut str 2) of
							Nothing -> NoMatch (start+2)
							Just n -> let
								size = 2+n+2
								end = start+size
								in Match [Token (Comment (Source.substr str 2 n)) (Source.IndexSpan start end)] (Source.precut str size, end)
	| (Source.isPrefixOf "//" str) = case Source.findLast (\c -> c /= '\n') (Source.precut str 2) of
							Nothing -> NoMatch start
							Just n -> let
								size = 2+n+1
								end = start+size
								in Match [Token (Comment (Source.substr str 2 (n+1))) (Source.IndexSpan start end)] (Source.precut str size, end)
	| otherwise = NoMatch start

consumeLiteral :: LexerFunc
consumeLiteral = foldr1 (>>>) literalFuncs
	where literalFuncs = map f (reverse (sortBy (\(x, _) (y, _) -> compare (length x) (length y)) literalMap))
		where f (lstr, ltok) (str, start)
			| not (Source.isPrefixOf lstr str) = NoMatch start
			| otherwise = let
				size = length lstr
				end = start + size
				in Match [Token ltok (Source.IndexSpan start end)] (Source.precut str size, end)

consumeInteger :: LexerFunc
consumeInteger (str, start)	= case Source.findLast isDigit str of
					Nothing -> NoMatch start
					Just n -> let
						size = n+1
						end = start+size
						in Match [Token (Integer (read (Source.substr str 0 size))) (Source.IndexSpan start end)] (Source.precut str size, end)

consumeIdentifier :: LexerFunc
consumeIdentifier (x:xs, start)
	| isAlpha x = f (x:xs, start) -- Also include first character for token Location
	| otherwise = NoMatch start
	where f (str, start)	= case Source.findLast (\c -> isAlphaNum c || c == '_') str of
					Nothing -> NoMatch start
					Just n -> let
						size = n+1
						end = start+size
						in Match [Token (Identifier (Source.substr str 0 size)) (Source.IndexSpan start end)] (Source.precut str size, end)

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
