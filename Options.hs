module Options (Warnings(..), Options(..), mkOptions) where

import System.Console.GetOpt

import PrettyPrinter

data Warnings = Warnings
	{ shadowing :: Bool
	}

allWarnings :: Warnings
allWarnings = Warnings
	{ shadowing = True
	}

data Options = Options
	{ astPrinter :: Printer (IO ())
	, showInput :: Bool
	, showLexingResult :: Bool
	, showParsingResult :: Bool
	, showAST :: Bool
	, lexOnly :: Bool
	, parseOnly :: Bool
	, enabledWarnings :: Warnings
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
	, enabledWarnings = allWarnings
	}

warningsOptions :: String -> Options -> Options
warningsOptions "no-shadow" o = o { enabledWarnings = (enabledWarnings o){ shadowing = False } }
warningsOptions other o = o

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
	, Option "W" [] (ReqArg warningsOptions "test") "Controls warnings (eg: -Wno-shadow), all warnings are enable by default"
	]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
	case getOpt Permute options argv of
		(o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "splang [OPTION...] file"
