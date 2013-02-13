import System.Environment

import qualified Lexer
import qualified Source
import ParserLib
import Parser
import Output


main :: IO ()
main = test

test = do
	[file] <- getArgs
	s <- readFile file
	putStrLn "File:"
	putStrLn s
	putStrLn "Lexing result:"
	case Lexer.lexer (s, 0) of
		Lexer.Match xs _ -> case filterComment xs of
			xs -> do
				print xs
				putStrLn "Parsing result:"
				case (parse parseProgram xs) of
					Left [x]		-> putStrLn (outputProgram x)
					Left xs			-> do
						putStrLn "Ambiguous input - able to derive multiple programs"
						putStrLn (concat (map ((\str -> "Option:\n" ++ str ++ "\n") . outputProgram) xs))
					Right EndOfStream	-> putStrLn "Error on end of stream"
					Right (Unexpected (Lexer.Token t l)) -> do	
						putStr "Unexpected token "
						putStr (show t)
						putStr " "
						Source.pointOutIndexSpan l s
		Lexer.NoMatch lError -> do
			putStr "Unexpected sequence of characters starting "
			Source.pointOutIndex lError s

convertToken :: String -> Lexer.Token Source.IndexSpan -> Lexer.Token Source.LocationSpan
convertToken s (Lexer.Token t (Source.IndexSpan from to)) = Lexer.Token t (Source.LocationSpan (Source.convert from s) (Source.convert to s))

filterComment :: [Lexer.Token a] -> [Lexer.Token a]
filterComment = filter (\t -> case t of
	Lexer.Token (Lexer.Comment _) _	-> False
	_				-> True
	)
