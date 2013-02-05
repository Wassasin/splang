import System.Environment
import Lexer

main :: IO ()
main = do
	[file] <- getArgs
	s <- readFile file
	putStrLn s