import Lexer

main :: IO ()
main = do
	s <- readFile "test.sl"
	putStrLn s