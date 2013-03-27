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
import Typing
import SemanticAnalysis
import Errors

standardMessage :: String -> String -> IndexSpan -> Console.MessageE -> String -> IO ()
standardMessage filename source idx kind message = do
	let loc = Source.convert (Source.beginOfSpan idx) source
	Console.putMessage kind filename loc message
	Source.pointOutIndexSpan idx source

main :: IO ()
main = do
	(opts, [file]) <- getArgs >>= mkOptions
	source <- readFile file
	when (showInput opts) $ putStrLn source

	lResult <- mlex opts file source
	when (lexOnly opts) $ exitSuccess

	pResult <- mparse opts file source (filterComment lResult)
	when (parseOnly opts) $ exitSuccess

	pResult2 <- midentifiers opts file source pResult

	pResult3 <- minfer opts file source pResult2

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
			standardMessage filename source (IndexSpan lError lError) Console.Error "Unexpected sequence of characters starting"
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
			sequence (interleave filename $ fmap (prettyPrint (astPrinter opts)) (take 2 ys))
			when (length ys > 2) (Console.putMessage Console.Note filename (-1, -1) (show ((length ys) - 2) ++ " more possible interpretations left out"))
			exitFailure
		Right EndOfStream	-> do
			putStrLn "Error on end of stream"
			exitFailure
		Right (Unexpected (Token t l)) -> do
			let loc = Source.convert (Source.beginOfSpan l) source
			standardMessage filename source l Console.Error ("Unexpected token " ++ show t)
			exitFailure

interleave file [] = []
interleave file (x:xs) = Console.putMessage Console.Note file (-1, -1) "Possible interpretation:" : x : interleave file xs

midentifiers :: Options -> String -> String -> (P1 Program) -> IO (P2 Program)
midentifiers opts filename source program = do
	let x = assignUniqueIDs program
	case x of
		Errors.Result newProgram [] warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			return newProgram
		Errors.Result _ errors warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			sequence $ map (printSemanticsError opts filename source) errors
			exitFailure
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			sequence $ map (printSemanticsError opts filename source) (errors ++ [fe])
			exitFailure

printSemanticsError :: Options -> String -> String -> ScopingError -> IO ()
printSemanticsError opts filename source (DuplicateDeclaration id1 gid) = case gid of
	Builtin b	-> do
		standardMessage filename source (src $ getMeta id1) Console.Error ("Redeclaration of builtin \"" ++ getString gid ++ "\"")
	User id2	-> do
		standardMessage filename source (src $ getMeta id1) Console.Error ("Redeclaration of identifier \"" ++ getString id1 ++ "\"")
		standardMessage filename source (src $ getMeta id2) Console.Note "Previous declaration here:"
printSemanticsError opts filename source (UndeclaredIdentifier ident context) = do
	let span = (src $ getMeta ident)
	let beginloc = Source.convert (Source.beginOfSpan span) source
	standardMessage filename source span Console.Error ("Undeclared identifier \"" ++ getString ident ++ "\"")
	case bestMatch ident context of
		Nothing -> return ()
		Just (User user, _) -> standardMessage filename source (src $ getMeta user) Console.Note ("Did you mean \"" ++ getString user ++ "\"?")
		Just (Builtin b, _) -> Console.putMessage Console.Note filename beginloc ("Did you mean \"" ++ getString b ++ "\"?")

printSemanticsWarning :: Options -> String -> String -> ScopingWarning -> IO ()
printSemanticsWarning opts filename source (ShadowsDeclaration id1 (User id2) scope) = ifWarning shadowing opts $ do
	standardMessage filename source (src $ getMeta id1) Console.Warning ("\"" ++ getString id1 ++ "\" shadows previous declaration.")
	standardMessage filename source (src $ getMeta id2) Console.Note (case scope of
		Global -> "Previous declaration was a global:"
		Argument -> "Previous declaration was used as argument to function:")
printSemanticsWarning opts filename source (ShadowsDeclaration id1 (Builtin b) scope) = ifWarning shadowing opts $ do
	standardMessage filename source (src $ getMeta id1) Console.Warning ("\"" ++ getString id1 ++ "\" shadows builtin.")


ifWarning kind opts = when (kind . enabledWarnings $ opts)

minfer :: Options -> String -> String -> (P2 Program) -> IO ()
minfer opts filename source program = do
	let x = infer program
	case x of
		Errors.Result _ [] [] -> do
			Console.highLight "Woehoe infering succeeded!"
		Errors.Result _ errors warnings -> do
			sequence $ map (printTypingError opts filename source) errors
			exitFailure
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printTypingError opts filename source) (fe:errors)
			exitFailure

printTypingError :: Options -> String -> String -> (InferError P2Meta) -> IO ()
printTypingError opts filename source (CannotUnify mt1 mt2)	= do
	standardMessage filename source (src2 $ getMeta mt1) Console.Error "Cannot unify types of"
	standardMessage filename source (src2 $ getMeta mt2) Console.Note "with"
printTypingError opts filename source (ContextNotFound ident)	= Console.putMessage Console.Error filename (-1, -1) ("Context not found: " ++ show ident)
printTypingError opts filename source (PolyViolation ft mt)	= standardMessage filename source (src2 $ getMeta mt) Console.Error "Polytype violation"
printTypingError opts filename source (UnknownIdentifier ident)	= standardMessage filename source (src2 $ getMeta ident) Console.Error ("Identifier is unknown: " ++ getString ident)
