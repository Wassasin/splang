import System.Environment

import Control.Monad
import System.Console.GetOpt

import qualified Lexer
import qualified Source
import ParserLib
import Parser
import PrettyPrinter
import qualified Console

data Options = Options
	{ astPrinter :: Printer (IO ())
	, showInput :: Bool
	, showLexingResult :: Bool
	, showParsingResult :: Bool
	}

defaultOptions :: Options
defaultOptions = Options
	{ astPrinter = coloredPrettyPrinter
	, showInput = False
	, showLexingResult = False
	, showParsingResult = False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option [] ["colored"] (NoArg (\o -> o { astPrinter = coloredPrettyPrinter })) "prints the AST with colored text"
	, Option [] ["plain"] (NoArg (\o -> o { astPrinter = plainPrettyPrinter })) "prints the AST in plain text"
	, Option [] ["minimizer"] (NoArg (\o -> o { astPrinter = miniPrettyPrinter })) "prints the AST without tabs and newlines"
	, Option [] ["show-input"] (NoArg (\o -> o { showInput = True })) "shows the input-file"
	, Option [] ["show-lexing"] (NoArg (\o -> o { showLexingResult = True })) "shows the in-between lexing result"
	, Option [] ["show-parsing"] (NoArg (\o -> o { showParsingResult = True })) "shows the in-between AST"
	]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
	case getOpt Permute options argv of
		(o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "slih [OPTION...] file"


main :: IO ()
main = test

test = do
	(opts, [file]) <- getArgs >>= mkOptions
	s <- readFile file
	when (showInput opts) $ Console.highLight "File:" >> putStrLn s

	when (showLexingResult opts) $ Console.highLight "Lexing result:"
	case Lexer.lexer (s, 0) of
		Lexer.Match xs _ -> let lResult = filterComment xs in do
				when (showLexingResult opts) $ print lResult
				when (showParsingResult opts) $ Console.highLight "Parsing result:"
				case (parse parseProgram xs) of
					Left [x]		-> when (showParsingResult opts) $ prettyPrint (astPrinter opts) x
					Left xs			-> do
						Console.putMessage Console.Error file (-1, -1) "Ambiguous input - able to derive multiple programs"
						--putStrLn (concat (map ((\str -> "Option:\n" ++ str ++ "\n") . (prettyPrint (astPrinter opts))) xs))
					Right EndOfStream	-> putStrLn "Error on end of stream"
					Right (Unexpected (Lexer.Token t l)) -> do
						Console.putMessage Console.Error file (-1, -1) ("Unexpected token " ++ show t)
						Source.pointOutIndexSpan l s

		Lexer.NoMatch lError -> do
			Console.putMessage Console.Error file (Source.convert lError s) "Unexpected sequence of characters starting"
			Source.pointOutIndex lError s

convertToken :: String -> Lexer.Token Source.IndexSpan -> Lexer.Token Source.LocationSpan
convertToken s (Lexer.Token t (Source.IndexSpan from to)) = Lexer.Token t (Source.LocationSpan (Source.convert from s) (Source.convert to s))

filterComment :: [Lexer.Token a] -> [Lexer.Token a]
filterComment = filter (\t -> case t of
	Lexer.Token (Lexer.Comment _) _	-> False
	_				-> True
	)
