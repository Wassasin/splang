import System.Environment

import qualified Lexer
import qualified Source
import Parser
import Output

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
				case (parse parseProgram file (filterComments xs)) of
					Right x -> putStrLn (outputProgram x)
					Left pError -> case (errorPos pError) of
						pPos -> do
							print pError
							Source.pointOutLocation (sourceLine pPos-1, sourceColumn pPos-1) s
		Lexer.NoMatch lError -> do
			putStr "Unexpected sequence of characters starting "
			Source.pointOutIndex lError s

filterComments :: [Lexer.Token a] -> [Lexer.Token a]
filterComments [] = []
filterComments ((Lexer.Token (Lexer.Comment _) _) : xs) = filterComments xs
filterComments (x:xs) = x : filterComments xs

convertToken :: String -> Lexer.Token Source.IndexSpan -> Lexer.Token Source.LocationSpan
convertToken s (Lexer.Token t (Source.IndexSpan from to)) = Lexer.Token t (Source.LocationSpan (Source.convert from s) (Source.convert to s))
