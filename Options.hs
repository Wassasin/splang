module Options (Warnings(..), Options(..), Target(..), mkOptions) where

import Control.Monad.Instances()
import System.Console.GetOpt

import PrettyPrinter

data Warnings = Warnings
	{ shadowing :: Bool
	}

allWarnings :: Warnings
allWarnings = Warnings
	{ shadowing = True
	}

data Target = SSM | LLVM
	deriving (Show, Eq)

data Options = Options
	{ astPrinter :: Printer (IO ())
	, showInput :: Bool
	, showLexingResult :: Bool
	, showParsingResult :: Bool
	, showScopingResult :: Bool
	, showTypingResult :: Bool
	, showIR :: Bool
	, showStages :: Bool
	, lexOnly :: Bool
	, parseOnly :: Bool
	, scopeOnly :: Bool
	, typeOnly :: Bool
	, forceCodegen :: Bool
	, enabledWarnings :: Warnings
	, target :: Target
	, outputFile :: String
	}

defaultOptions :: Options
defaultOptions = Options
	{ astPrinter = coloredPrettyPrinter
	, showInput = False
	, showLexingResult = False
	, showParsingResult = False
	, showScopingResult = False
	, showTypingResult = False
	, showIR = False
	, showStages = False
	, lexOnly = False
	, parseOnly = False
	, scopeOnly = False
	, typeOnly = False
	, forceCodegen = False
	, enabledWarnings = allWarnings
	, target = SSM
	, outputFile = ""
	}

warningsOptions :: String -> Warnings -> Warnings
warningsOptions "no-shadow" w = w { shadowing = False }
warningsOptions _ w = w

-- Apply changes on Warnings to Options
liftW :: (Warnings -> Warnings) -> (Options -> Options)
liftW f o = o { enabledWarnings = f (enabledWarnings o) }

targetOptions :: String -> Target -> Target
targetOptions "ssm" _ = SSM
targetOptions "llvm" _ = LLVM
targetOptions _ _ = error "Unrecognized target specification"

liftT :: (Target -> Target) -> (Options -> Options)
liftT f o = o { target = f (target o) }

options :: [OptDescr (Options -> Options)]
options =
	[ Option [] ["colored"]		(NoArg (\o -> o { astPrinter = coloredPrettyPrinter }))	"prints the AST with colored text"
	, Option [] ["plain"]		(NoArg (\o -> o { astPrinter = plainPrettyPrinter }))	"prints the AST in plain text"
	, Option [] ["minimizer"]	(NoArg (\o -> o { astPrinter = miniPrettyPrinter }))	"prints the AST without tabs and newlines"
	, Option [] ["show-input"]	(NoArg (\o -> o { showInput = True }))			"shows the input-file"
	, Option [] ["show-lexing"]	(NoArg (\o -> o { showLexingResult = True }))		"shows the in-between lexing result"
	, Option [] ["show-parsing"]	(NoArg (\o -> o { showParsingResult = True }))		"prettyprints the AST after parsing"
	, Option [] ["show-scoping"]	(NoArg (\o -> o { showScopingResult = True }))		"prettyprints the AST after scoping"
	, Option [] ["show-typing"]	(NoArg (\o -> o { showTypingResult = True }))		"prettyprints the AST after typing"
	, Option [] ["show-ir"]		(NoArg (\o -> o { showIR = True }))			"prints the IR (both plain and canonical)"
	, Option [] ["show-stages"]	(NoArg (\o -> o { showStages = True }))			"show stages during compilation"
	, Option [] ["lex-only"]	(NoArg (\o -> o { lexOnly = True }))			"stops after the lexing pass"
	, Option [] ["parse-only"]	(NoArg (\o -> o { parseOnly = True }))			"stops after the parsing pass"
	, Option [] ["scope-only"]	(NoArg (\o -> o { scopeOnly = True }))			"stops after the scoping pass"
	, Option [] ["type-only"]	(NoArg (\o -> o { typeOnly = True }))			"stops after the typing pass"
	, Option [] ["force-codegen"]	(NoArg (\o -> o { forceCodegen = True }))		"will generate code, even when there are (non-fatal) errors in analysis"
	, Option "W" []			(ReqArg (fmap liftW warningsOptions) "warning")		"Controls warnings (eg: -Wno-shadow), all warnings are enable by default"
	, Option [] ["target"]		(ReqArg (fmap liftT targetOptions) "target")		"Specifies target (eg: --target llvm), default is ssm"
	, Option "o" []			(ReqArg (\s o -> o { outputFile = s }) "filename")	"Specifies output file (don't specify for stdout)"
	]

mkOptions :: [String] -> IO (Options, [String])
mkOptions argv =
	case getOpt Permute options argv of
		(o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "splang [OPTION...] file"
