import System.Environment
import Lexer

main :: IO ()
main = do
	[file] <- getArgs
	s <- readFile file
	case lexer (s, 0) of
		Match xs _ -> print $ xs
		NoMatch n -> print $ n
