import Lexer

main :: IO ()
main = do
	s <- readFile "test.sl"
	case lexer (s, 0) of
		Match xs _ -> print $ xs
		NoMatch n -> print $ n
