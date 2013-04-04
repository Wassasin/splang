import System.Environment

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
import PrettyPrinter
import TypePrinter
import TypeInference
import SemanticAnalysis
import Errors

standardMessageIO :: String -> String -> IndexSpan -> Console.MessageE -> IO () -> IO ()
standardMessageIO filename source idx kind message = do
	let loc = Source.convert (Source.beginOfSpan idx) source
	Console.putMessage kind filename loc ""
	message
	putStr "\n"
	Source.pointOutIndexSpan idx source

standardMessage :: String -> String -> IndexSpan -> Console.MessageE -> String -> IO ()
standardMessage filename source idx kind message = do
	standardMessageIO filename source idx kind (Console.intense message)

show2 :: Show a => GeneralIdentifier a -> String
show2 (User (AST.Identifier str _ _)) = "User " ++ str
show2 s = show s

getFunDeclContext :: P2 Decl -> P1Context
getFunDeclContext (FunDecl _ _ _ _ stmts _) = context $ getMeta $ last stmts

printContext :: P1Context -> IO ()
printContext context = do
	sequence $ map (\(i, (s, t)) -> do
				Console.intense (show s ++ ": " ++ show2 i ++ " :: ")
				polyTypePrint coloredTypePrinter t
				putStr "\n") context
	return ()

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
	when (scopeOnly opts) $ exitSuccess

	Console.highLightLn "Global"
	let globalContext = (context (getMeta pResult2))
	printContext globalContext

	let (Program decls _) = pResult2
	let fundecls = filter (\a -> case a of
		FunDecl _ _ _ _ _ _ -> True
		VarDecl _ _ _ _ -> False) decls
	let funcontexts = map (\c -> c \\ globalContext) $ map getFunDeclContext fundecls
	let funnames = map (getString . getIdentifier) fundecls
	sequence $ (concat . transpose) [(map Console.highLightLn funnames), (map printContext funcontexts)]

	pResult3 <- minfer opts file source pResult2

	Console.highLightLn "Globals infered:"
	sequence $ map (\(i, t) -> do
		Console.intense (show i ++ ": ")
		polyTypePrint coloredTypePrinter t
		putStr "\n") pResult3

	exitSuccess

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
			exitFailure

-- filename and source are needed for error-messaging! Returns an AST or errors.
mparse :: Options -> String -> String -> [Token] -> IO (P1 Program)
mparse opts filename source tokens = do
	when (showParsingResult opts) $ Console.highLightLn "Parsing result:"
	let pResult = parseSPL tokens
	case pResult of
		Left [x]			-> do
			when (showParsingResult opts) $ prettyPrint (astPrinter opts) x
			return x
		Left ys				-> do
			Console.putMessageLn Console.Error filename (-1, -1) "Ambiguous input - able to derive multiple programs"
			sequence (interleave filename $ fmap (prettyPrint (astPrinter opts)) (take 2 ys))
			when (length ys > 2) (Console.putMessageLn Console.Note filename (-1, -1) (show ((length ys) - 2) ++ " more possible interpretations left out"))
			exitFailure
		Right EndOfStream		-> do
			putStrLn "Error on end of stream"
			exitFailure
		Right (Unexpected (Token t l))	-> do
			standardMessage filename source l Console.Error ("Unexpected token " ++ show t)
			exitFailure
		Right Ambiguity			-> do
			Console.putMessage Console.Error filename (-1, -1) "COMPILER BUG: Ambiguous input, without results!"
			exitFailure

interleave file [] = []
interleave file (x:xs) = Console.putMessageLn Console.Note file (-1, -1) "Possible interpretation:" : x : interleave file xs

midentifiers :: Options -> String -> String -> (P1 Program) -> IO (P2 Program)
midentifiers opts filename source program = do
	let x = assignUniqueIDs program
	case x of
		Errors.Result newProgram [] warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			when (showScopingResult opts) $ prettyPrint (astPrinter opts) newProgram
			return newProgram
		Errors.Result newProgram errors warnings -> do
			sequence $ map (printSemanticsWarning opts filename source) warnings
			sequence $ map (printSemanticsError opts filename source) errors
			when (showParsingResult opts) $ prettyPrint (astPrinter opts) newProgram
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

minfer :: Options -> String -> String -> (P2 Program) -> IO [(AST.IdentID, PolyType P2Meta)]
minfer opts filename source program = do
	let x = infer program
	case x of
		Errors.Result (cs, _) [] [] -> do
			return cs
		Errors.Result _ errors warnings -> do
			sequence $ map (printTypingError opts filename source) errors
			exitFailure
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printTypingError opts filename source) (fe:errors)
			exitFailure

printTypingError :: Options -> String -> String -> (InferError P2Meta) -> IO ()
printTypingError opts filename source (CannotUnify mt1 mt2)	= do
	Console.putMessage Console.Error filename (-1, -1) "Cannot unify types "
	monoTypePrint coloredTypePrinter mt1
	Console.intense " and "
	monoTypePrint coloredTypePrinter mt2
	putStr "\n"
	standardMessageIO filename source (src2 $ getMeta mt1) Console.Note (do
		Console.intense "Type "
		monoTypePrint coloredTypePrinter mt1
		Console.intense " inferred here:")
	standardMessageIO filename source (src2 $ getMeta mt2) Console.Note (do
		Console.intense "Type "
		monoTypePrint coloredTypePrinter mt2
		Console.intense " inferred here:")
printTypingError opts filename source (ContextNotFound ident)	= Console.putMessageLn Console.Error filename (-1, -1) ("Context not found: " ++ show ident)
printTypingError opts filename source (PolyViolation ft mt)	= do
	standardMessageIO filename source (src2 $ getMeta ft) Console.Error (do
		Console.intense "Attempted to substitute a bound free type variable "
		monoTypePrint coloredTypePrinter (Free ft $ getMeta ft)
		Console.intense " in a PolyType. "
		Console.highLight "COMPILER BUG")
	standardMessageIO filename source (src2 $ getMeta mt) Console.Note (do
		Console.intense "With type "
		monoTypePrint coloredTypePrinter mt
		Console.intense " which was inferred from here:")
printTypingError opts filename source (UnknownIdentifier ident)	= standardMessage filename source (src2 $ getMeta ident) Console.Error ("Identifier is unknown: " ++ getString ident)
printTypingError opts filename source (VoidUsage meta mt)	= do
	standardMessage filename source (src2 $ meta) Console.Error "Using void"
	standardMessageIO filename source (src2 $ getMeta mt) Console.Note (do
		Console.intense "Type "
		monoTypePrint coloredTypePrinter mt
		Console.intense " inferred here:")
printTypingError opts filename source (TypeError pt1 pt2)	= do
	standardMessageIO filename source (src2 $ getMeta pt1) Console.Note (do
		Console.intense "Type mismatch. Expected type "
		polyTypePrint coloredTypePrinter pt1
		Console.intense " declared here:")
	standardMessageIO filename source (src2 $ getMeta pt2) Console.Note (do
		Console.intense "Actual type "
		polyTypePrint coloredTypePrinter pt2
		Console.intense " inferred here:")