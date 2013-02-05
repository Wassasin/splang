import Lexer

main = case lexer ("if true /* altijd waar! */ return false;", 0) of
	Match xs _ -> print $ xs
	NoMatch n -> print $ n
