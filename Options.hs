module Options (Warnings(..), Options(..), mkOptions) where

import Control.Monad.Instances
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
	, showScopingResult :: Bool
	, lexOnly :: Bool
	, parseOnly :: Bool
	, scopeOnly :: Bool
	, enabledWarnings :: Warnings
	}

defaultOptions :: Options
defaultOptions = Options
	{ astPrinter = coloredPrettyPrinter
	, showInput = False
	, showLexingResult = False
	, showParsingResult = False
	, showScopingResult = False
	, lexOnly = False
	, parseOnly = False
	, scopeOnly = False
	, enabledWarnings = allWarnings
	}

warningsOptions :: String -> Warnings -> Warnings
warningsOptions "no-shadow" w = w { shadowing = False }
warningsOptions other w = w

-- Apply changes on Warnings to Options
lift :: (Warnings -> Warnings) -> (Options -> Options)
lift f o = o { enabledWarnings = f (enabledWarnings o) }

options :: [OptDescr (Options -> Options)]
options =
	[ Option [] ["colored"]		(NoArg (\o -> o { astPrinter = coloredPrettyPrinter }))	"prints the AST with colored text"
	, Option [] ["plain"]		(NoArg (\o -> o { astPrinter = plainPrettyPrinter }))	"prints the AST in plain text"
	, Option [] ["minimizer"]	(NoArg (\o -> o { astPrinter = miniPrettyPrinter }))	"prints the AST without tabs and newlines"
	, Option [] ["show-input"]	(NoArg (\o -> o { showInput = True }))			"shows the input-file"
	, Option [] ["show-lexing"]	(NoArg (\o -> o { showLexingResult = True }))		"shows the in-between lexing result"
	, Option [] ["show-parsing"]	(NoArg (\o -> o { showParsingResult = True }))		"prettyprints the AST after parsing"
	, Option [] ["show-scoping"]	(NoArg (\o -> o { showScopingResult = True }))		"prettyprints the AST after scoping"
	, Option [] ["lex-only"]	(NoArg (\o -> o { lexOnly = True }))			"stops after the lexing pass"
	, Option [] ["parse-only"]	(NoArg (\o -> o { parseOnly = True }))			"stops after the parsing pass"
	, Option [] ["scope-only"]	(NoArg (\o -> o { scopeOnly = True }))			"stops after the scoping pass"
	, Option "W" []			(ReqArg (fmap lift warningsOptions) "warning")		"Controls warnings (eg: -Wno-shadow), all warnings are enable by default"
	]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
	case getOpt Permute options argv of
		(o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "splang [OPTION...] file"
