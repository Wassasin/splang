import System.Environment

import System.Exit
import Control.Monad
import Text.EditDistance
import Data.List
import Data.Ord

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

bestMatch :: [AST.Identifier a] -> AST.Identifier a -> Maybe (AST.Identifier a)
bestMatch l search = let (cost, best) = minimumBy (comparing fst) . map (\ident -> (restrictedDamerauLevenshteinDistance defaultEditCosts (getIdentifierString search) (getIdentifierString ident), ident)) $ l in
	if cost<3 then Just best else Nothing

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
	when (showAST opts) $ print (fmap (const ()) pResult2)

	Console.highLight "Debug result:"
	prettyPrint (astPrinter opts) pResult2
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

midentifiers :: Options -> String -> String -> (P1 Program) -> IO (P2 Program)
midentifiers opts filename source program = do
	let x = assignUniqueIDs program
	case x of
		Errors.Result newProgram [] warnings -> do
			sequence $ map (printSemanticsWarning filename source) warnings
			return newProgram
		Errors.Result _ errors warnings -> do
			sequence $ map (printSemanticsWarning filename source) warnings
			sequence $ map (printSemanticsError filename source) errors
			exitFailure
		Errors.FatalError fe errors warnings -> do
			sequence $ map (printSemanticsWarning filename source) warnings
			sequence $ map (printSemanticsError filename source) (errors ++ [fe])
			exitFailure

printSemanticsError :: String -> String -> ScopingError -> IO ()
printSemanticsError filename source (DuplicateDeclaration id1 id2) = do
	standardMessage filename source (src $ getMeta id1) Console.Error ("Redeclaration of identifier \"" ++ getIdentifierString id1 ++ "\"")
	standardMessage filename source (src $ getMeta id2) Console.Note "Previous declaration here:"
printSemanticsError filename source (UndeclaredIdentifier ident context) = do
	standardMessage filename source (src $ getMeta ident) Console.Error ("Undeclared identifier \"" ++ getIdentifierString ident ++ "\"")
	case bestMatch (fst (unzip context)) ident of
		Nothing -> return ()
		Just bm -> standardMessage filename source (src $ getMeta bm) Console.Note ("Did you mean \"" ++ getIdentifierString bm ++ "\"?")

interleave file [] = []
interleave file (x:xs) = Console.putMessage Console.Note file (-1, -1) "Possible interpretation:" : x : interleave file xs

printSemanticsWarning :: String -> String -> ScopingWarning -> IO ()
printSemanticsWarning filename source (ShadowsDeclaration id1 id2 scope) = do
	standardMessage filename source (src $ getMeta id1) Console.Warning ("\"" ++ getIdentifierString id1 ++ "\" shadows previous declaration.")
	standardMessage filename source (src $ getMeta id2) Console.Note (case scope of
		Global -> "Previous declaration was a global:"
		Argument -> "Previous declaration was used as argument to function:")
