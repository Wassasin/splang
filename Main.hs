import System.Environment
import qualified Lexer
import Parser
import Text.Parsec


main :: IO ()
main = do
	[file] <- getArgs
	s <- readFile file
	putStrLn "File:"
	putStrLn s
	putStrLn "Lexing result:"
	case Lexer.lexer (s, 0) of
		Lexer.Match xs _ -> do
			print xs
			putStrLn "Parsing result:"
			case (parse parseTerm4 file xs) of
				Right x -> print x
				Left pError -> print pError
		Lexer.NoMatch lError -> print lError
