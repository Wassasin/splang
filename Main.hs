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
		Lexer.Match xs _ -> do
			print xs
			putStrLn "Parsing result:"
			case (parse parseProgram file xs) of
				Right x -> print x
				Left pError -> case (errorPos pError) of
					pPos -> do
						print pError
						putStr (show (map messageString (Text.Parsec.Error.errorMessages pError)))
						Source.pointOutLocation (sourceLine pPos-1, sourceColumn pPos-1) s
		Lexer.NoMatch lError -> do
			putStr "Unexpected sequence of characters starting "
			Source.pointOutIndex lError s
