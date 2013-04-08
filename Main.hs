import System.Environment

import Data.Maybe (fromJust)
import Data.List
import System.Exit
import Control.Monad

import qualified Console
import Options
import Source
import Lexer
import Parser
import AST
import Meta
import Output
import PrettyPrinter
import TypePrinter
import TypeInference
import SemanticAnalysis
import Errors

valid :: IndexSpan -> Bool
valid (IndexSpan n m) = n >= 0 && m >= n

standardMessageIO :: String -> String -> IndexSpan -> Console.MessageE -> IO () -> IO ()
standardMessageIO filename source idx kind message = do
	when (valid idx) $ do
		let loc = Source.convert (Source.beginOfSpan idx) source
		Console.putMessage kind filename loc ""
		message
		putStr "\n"
		Source.pointOutIndexSpan idx source
	when (not $ valid idx) $ do
		Console.putMessage kind filename (-1,-1) ""
		message
		putStr " (no location)\n"

standardMessage :: String -> String -> IndexSpan -> Console.MessageE -> String -> IO ()
standardMessage filename source idx kind message = do
	standardMessageIO filename source idx kind (Console.intense message)

show2 :: Show a => GeneralIdentifier a -> String
show2 (User (AST.Identifier str _ _)) = "User " ++ str
show2 s = show s

getFunDeclContext :: P2 Decl -> P1Context
getFunDeclContext (FunDecl _ _ _ _ stmts _) = context $ getMeta $ last stmts

exitFatal :: IO a
exitFatal = do
	Console.highLightLn "*** Stopping due to fatal error"
	exitFailure

exit :: Bool -> IO ()
exit True = exitSuccess
exit False = exitFailure

sucfail :: Bool -> String
sucfail True = "succeeded"
sucfail False = "failed (but we try to continue!)"

main :: IO ()
main = do
	(opts, [file]) <- getArgs >>= mkOptions
	when (showStages opts) $ Console.highLightLn ("*** Compiling " ++ file ++ " ***")

	source <- readFile file
	when (showInput opts) $ putStrLn source
	when (showStages opts) $ Console.highLightLn "*** Done reading file"

	lResult <- mlex opts file source
	when (showStages opts) $ Console.highLightLn ("*** Lexing is done")
	when (lexOnly opts) $ exitSuccess

	(pResult, b0) <- mparse opts file source (filterComment lResult)
	when (showStages opts) $ Console.highLightLn ("*** Parsing " ++ sucfail b0)
	when (parseOnly opts) $ exit b0

	(pResult2, b1) <- midentifiers opts file source pResult
	when (showStages opts) $ Console.highLightLn ("*** Scoping " ++ sucfail b1)
	when (scopeOnly opts) $ exit b1

	(pResult3, b2) <- minfer opts file source pResult2
	when (showStages opts) $ Console.highLightLn ("*** Typing " ++ sucfail b2)

	when (showStages opts) $ Console.highLightLn ("*** Done ")

	let b = b0 && b1 && b2
	exit b

-- filename is needed for error-messaging! Returns a list of tokens or errors.
mlex :: Options -> String -> String -> IO [Token]
mlex opts filename source = do
	when (showLexingResult opts) $ Console.highLightLn "Lexing result:"
	let lResult = lexer (source, 0)
	case lResult of
		Match xs _ -> do
			when (showLexingResult opts) $ print xs
			return xs
		NoMatch lError -> do
			standardMessage filename source (IndexSpan lError lError) Console.Error "Unexpected sequence of characters starting"
			exitFatal

-- filename and source are needed for error-messaging! Returns an AST or errors.
mparse :: Options -> String -> String -> [Token] -> IO (P1 Program, Bool)
mparse opts filename source tokens = do
	when (showParsingResult opts) $ Console.highLightLn "Parsing result:"
	let pResult = parseSPL tokens
	case pResult of
		Left [x]			-> do
			when (showParsingResult opts) $ prettyPrint basicInfo (astPrinter opts) x
			return (x, True)
		Left ys				-> do
			Console.putMessageLn Console.Error filename (-1, -1) "Ambiguous input - able to derive multiple programs"
			sequence (interleave filename $ fmap (prettyPrint basicInfo (astPrinter opts)) (take 2 ys))
			when (length ys > 2) (Console.putMessageLn Console.Note filename (-1, -1) (show ((length ys) - 2) ++ " more possible interpretations left out"))
			return (head ys, False)
		Right EndOfStream		-> do
			putStrLn "Error on end of stream"
			exitFatal
		Right (Unexpected (Token t l))	-> do
			standardMessage filename source l Console.Error ("Unexpected token " ++ show t)
			exitFatal
		Right Ambiguity			-> do
			Console.putMessage Console.Error filename (-1, -1) "COMPILER BUG: Ambiguous input, without results!"
			exitFatal

interleave file [] = []
interleave file (x:xs) = Console.putMessageLn Console.Note file (-1, -1) "Possible interpretation:" : x : interleave file xs

midentifiers :: Options -> String -> String -> (P1 Program) -> IO (P2 Program, Bool)
midentifiers opts filename source program = do
	let x = assignUniqueIDs program
	case x of
		Errors.Result newProgram [] warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			when (showScopingResult opts) $ prettyPrint (withDeclCommentLine declComments basicInfo) (astPrinter opts) newProgram
			return (newProgram, True)
		Errors.Result newProgram errors warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			sequence $ map (printSemanticsError opts filename source) errors
			when (showParsingResult opts) $ prettyPrint (withDeclCommentLine declComments basicInfo) (astPrinter opts) newProgram
			return (newProgram, False)
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			sequence $ map (printSemanticsError opts filename source) (errors ++ [fe])
			exitFatal

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
		Just (Builtin b, _) -> Console.putMessageLn Console.Note filename beginloc ("Did you mean \"" ++ getString b ++ "\"?")

printSemanticsWarning :: Options -> String -> String -> ScopingWarning -> IO ()
printSemanticsWarning opts filename source (ShadowsDeclaration id1 (User id2) scope) = ifWarning shadowing opts $ do
	standardMessage filename source (src $ getMeta id1) Console.Warning ("\"" ++ getString id1 ++ "\" shadows previous declaration.")
	standardMessage filename source (src $ getMeta id2) Console.Note (case scope of
		Global -> "Previous declaration was a global:"
		Argument -> "Previous declaration was used as argument to function:"
		Local -> "COMPILER BUG: shadowing locals doesn't make sense!")
printSemanticsWarning opts filename source (ShadowsDeclaration id1 (Builtin b) scope) = ifWarning shadowing opts $ do
	standardMessage filename source (src $ getMeta id1) Console.Warning ("\"" ++ getString id1 ++ "\" shadows builtin.")

ifWarning kind opts = when (kind . enabledWarnings $ opts)

declComments :: P2 AST.Decl  -> MarkupString Styles
declComments decl = lift (getString (getIdentifier decl) ++ " :: ") ++ output basicInfo (fromJust . annotatedType $ getMeta decl)

minfer :: Options -> String -> String -> (P2 Program) -> IO ([(AST.IdentID, PolyType P2Meta)], Bool)
minfer opts filename source program = do
	let x = infer program
	case x of
		Errors.Result (cs, _) [] [] -> do
			return (cs, True)
		Errors.Result (cs, _) errors warnings -> do
			sequence $ map (printTypingError opts filename source) errors
			return (cs, False)
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printTypingError opts filename source) (fe:errors)
			exitFatal

printTypingError :: Options -> String -> String -> (InferError P2Meta) -> IO ()
printTypingError opts filename source (CannotUnify m mt1 mt2)	= do
	standardMessageIO filename source (src m) Console.Error (do
		Console.intense "Cannot unify types "
		monoTypePrint basicInfo coloredTypePrinter mt1
		Console.intense " and "
		monoTypePrint basicInfo coloredTypePrinter mt2)
	standardMessageIO filename source (src $ getMeta mt1) Console.Note (do
		Console.intense "Type "
		monoTypePrint basicInfo coloredTypePrinter mt1
		Console.intense " inferred here:")
	standardMessageIO filename source (src $ getMeta mt2) Console.Note (do
		Console.intense "Type "
		monoTypePrint basicInfo coloredTypePrinter mt2
		Console.intense " inferred here:")
printTypingError opts filename source (ContextNotFound ident)	= do
	let loc = (-1,-1)
	Console.putMessageLn Console.Error filename loc ("Context not found: " ++ show ident)
	Console.putMessageLn Console.Note filename loc "This is probably caused by earlier errors"
printTypingError opts filename source (UnknownIdentifier ident)	= do
	let loc = Source.beginOfSpan $ src $ getMeta ident
	standardMessage filename source (src $ getMeta ident) Console.Error ("Identifier is unknown: " ++ getString ident)
	Console.putMessageLn Console.Note filename (Source.convert loc source) "This is probably caused by earlier errors"
printTypingError opts filename source (VoidUsage meta mt)	= do
	standardMessage filename source (src $ meta) Console.Error "Using void"
	standardMessageIO filename source (src $ getMeta mt) Console.Note (do
		Console.intense "Type "
		monoTypePrint basicInfo coloredTypePrinter mt
		Console.intense " inferred here:")
printTypingError opts filename source (TypeError pt1 pt2)	= do
	standardMessageIO filename source (src $ getMeta pt1) Console.Error (do
		Console.intense "Type mismatch. Expected type "
		monoTypePrint basicInfo coloredTypePrinter pt1
		Console.intense " declared here:")
	standardMessageIO filename source (src $ getMeta pt2) Console.Note (do
		Console.intense "Actual type "
		monoTypePrint basicInfo coloredTypePrinter pt2
		Console.intense " inferred here:")
printTypingError opts filename source (WrongArguments es as m)	= do 
	standardMessage filename source (src m) Console.Error (pre ++ show (length es) ++ " given, but " ++ show (length as) ++ " expected.")
	where pre = if (length es < length as) then "Too few arguments given, " else "Too many arguments given, "
printTypingError opts filename source (NoFunction i u m)	= standardMessageIO filename source (src m) Console.Error (do
		Console.intense $ getString i ++ " used as function, but has type "
		monoTypePrint basicInfo coloredTypePrinter u)