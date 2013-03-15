module Options (Options(..), mkOptions) where

import System.Console.GetOpt

import PrettyPrinter

data Options = Options
	{ astPrinter :: Printer (IO ())
	, showInput :: Bool
	, showLexingResult :: Bool
	, showParsingResult :: Bool
	, showAST :: Bool
	, lexOnly :: Bool
	, parseOnly :: Bool
	}

defaultOptions :: Options
defaultOptions = Options
	{ astPrinter = coloredPrettyPrinter
	, showInput = False
	, showLexingResult = False
	, showParsingResult = False
	, showAST = False
	, lexOnly = False
	, parseOnly = False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option [] ["colored"] (NoArg (\o -> o { astPrinter = coloredPrettyPrinter })) "prints the AST with colored text"
	, Option [] ["plain"] (NoArg (\o -> o { astPrinter = plainPrettyPrinter })) "prints the AST in plain text"
	, Option [] ["minimizer"] (NoArg (\o -> o { astPrinter = miniPrettyPrinter })) "prints the AST without tabs and newlines"
	, Option [] ["show-input"] (NoArg (\o -> o { showInput = True })) "shows the input-file"
	, Option [] ["show-lexing"] (NoArg (\o -> o { showLexingResult = True })) "shows the in-between lexing result"
	, Option [] ["show-parsing"] (NoArg (\o -> o { showParsingResult = True })) "prettyprints the AST"
	, Option [] ["show-splast"] (NoArg (\o -> o { showAST = True })) "dumps the in-between AST after some pass"
	, Option [] ["lex-only"] (NoArg (\o -> o { lexOnly = True })) "stops after the lexing pass"
	, Option [] ["parse-only"] (NoArg (\o -> o { parseOnly = True })) "stops after the parsing pass"
	]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
	case getOpt Permute options argv of
		(o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "splang [OPTION...] file"
