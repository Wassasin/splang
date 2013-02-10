import System.Environment

import qualified Lexer
import qualified Source
import Parser

import Text.Parsec
import Text.Parsec.Error


main :: IO ()
main = test

test = do
	[file] <- getArgs
	s <- readFile file
	putStrLn "File:"
	putStrLn s
	putStrLn "Lexing result:"
	case Lexer.lexer (s, 0) of
		Lexer.Match xs _ -> case map (convertToken s) xs of
			xs -> do
				print xs
				putStrLn "Parsing result:"
				case (parse parseProgram file xs) of
					Right x -> print x
					Left pError -> case (errorPos pError) of
						pPos -> do
							print pError
							Source.pointOutLocation (sourceLine pPos-1, sourceColumn pPos-1) s
		Lexer.NoMatch lError -> do
			putStr "Unexpected sequence of characters starting "
			Source.pointOutIndex lError s

convertToken :: String -> Lexer.Token Source.IndexSpan -> Lexer.Token Source.LocationSpan
convertToken s (Lexer.Token t (Source.IndexSpan from to)) = Lexer.Token t (Source.LocationSpan (Source.convert from s) (Source.convert to s))
