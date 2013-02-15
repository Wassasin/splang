import System.Environment

import System.Exit
import Control.Monad

import qualified Console
import Options
import Source
import Lexer
import Parser
import AST
import Meta
import PrettyPrinter


main :: IO ()
main = do
	(opts, [file]) <- getArgs >>= mkOptions
	source <- readFile file

	lResult <- mlex opts file source
	pResult <- mparse opts file source (filterComment lResult)
	when (showAST opts) $ print pResult
	exitSuccess

-- filename is needed for error-messaging! Returns a list of tokens or errors.
mlex :: Options -> String -> String -> IO [Token]
mlex opts filename source = do
	when (showLexingResult opts) $ Console.highLight "Lexing result:"
	let lResult = lexer (source, 0)
	case lResult of
		Match xs _ -> do
			when (showLexingResult opts) $ print xs
			return xs
		NoMatch lError -> do
			Console.putMessage Console.Error filename (Source.convert lError source) "Unexpected sequence of characters starting"
			Source.pointOutIndex lError source
			exitFailure

-- filename and source are needed for error-messaging! Returns an AST or errors.
mparse :: Options -> String -> String -> [Token] -> IO (P1 Program)
mparse opts filename source tokens = do
	when (showParsingResult opts) $ Console.highLight "Parsing result:"
	let pResult = parseSPL tokens
	case pResult of
		Left [x]			-> do
			when (showParsingResult opts) $ prettyPrint (astPrinter opts) x
			return x
		Left ys				-> do
			Console.putMessage Console.Error filename (-1, -1) "Ambiguous input - able to derive multiple programs"
			sequence (interleave filename $ fmap (prettyPrint (astPrinter opts)) ys)
			exitFailure
		Right EndOfStream	-> do
			putStrLn "Error on end of stream"
			exitFailure
		Right (Unexpected (Token t l)) -> do
			let loc = Source.convert (case l of Source.IndexSpan start _ -> start) source
			Console.putMessage Console.Error filename loc ("Unexpected token " ++ show t)
			Source.pointOutIndexSpan l source
			exitFailure

interleave file [] = []
interleave file (x:xs) = Console.putMessage Console.Note file (-1, -1) "Possible interpretation:" : x : interleave file xs
