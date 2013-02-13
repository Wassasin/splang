import System.Environment

import qualified Lexer
import qualified Source
import Parser
import PrettyPrinter
import qualified Console

import Text.Parsec
import Text.Parsec.Error

main :: IO ()
main = test

test = do
	[file] <- getArgs
	s <- readFile file
	Console.highLight "File:"
	putStrLn s
	Console.highLight "Lexing result:"
	case Lexer.lexer (s, 0) of
		Lexer.Match xs _ -> let lResult = map (convertToken s) xs in do
				print lResult
				Console.highLight "Parsing result:"
				case (parse parseProgram file (filterComments lResult)) of
					Right x -> prettyPrint coloredPrettyPrinter x
					Left pError -> let
						pPos = (errorPos pError)
						loc = (sourceLine pPos-1, sourceColumn pPos-1) in do
						Console.putMessage Console.Error file loc (messageString $ head $ errorMessages pError) -- parsec errors are stupid
						Source.pointOutLocation loc s
		Lexer.NoMatch lError -> do
			Console.putMessage Console.Error file (Source.convert lError s) "Unexpected sequence of characters starting"
			Source.pointOutIndex lError s

filterComments :: [Lexer.Token a] -> [Lexer.Token a]
filterComments [] = []
filterComments ((Lexer.Token (Lexer.Comment _) _) : xs) = filterComments xs
filterComments (x:xs) = x : filterComments xs

convertToken :: String -> Lexer.Token Source.IndexSpan -> Lexer.Token Source.LocationSpan
convertToken s (Lexer.Token t (Source.IndexSpan from to)) = Lexer.Token t (Source.LocationSpan (Source.convert from s) (Source.convert to s))

