module Output (Styles(..), OpenClose(..), Markup, MarkupString, OutputInfo, lift, basicInfo, withDeclCommentLine, outputProgram, outputMonoType, outputPolyType) where

import AST
import qualified Typing

data Styles = Type
	| Variable
	| Constant
	| Keyword
	| Function
	| UniqueID
	| Comments

data OpenClose a = Open a | Close a
type Markup a = Either Char (OpenClose a)
type MarkupString a = [Markup a]

data OutputInfo a = OutputInfo
	{ declComment :: Decl a -> MarkupString Styles
	, indentation :: Int
	, brackets :: Bool }

lift :: String -> MarkupString a
lift = fmap Left

erase :: MarkupString a -> String
erase [] = []
erase (Left c : xs) = c : erase xs
erase (Right _: xs) = erase xs

open :: a -> MarkupString a
open a = return $ Right $ Open a

close :: a -> MarkupString a
close a = return $ Right $ Close a

getIdentifierUniqueID :: Identifier a -> String
getIdentifierUniqueID (Identifier _ (Just n) _) = show n
getIdentifierUniqueID (Identifier _ Nothing _) = ""

keyword str = open Keyword ++ lift str ++ close Keyword
constant str = open Constant ++ lift str ++ close Constant
variable ident = open Variable ++ lift (getIdentifierString ident) ++ close Variable ++ open UniqueID ++ lift (getIdentifierUniqueID ident) ++ close UniqueID
function ident = open Function ++ lift (getIdentifierString ident) ++ close Function ++ open UniqueID ++ lift (getIdentifierUniqueID ident) ++ close UniqueID

basicInfo :: OutputInfo a
basicInfo = OutputInfo
	{ declComment = (\_ -> lift "")
	, indentation = 0
	, brackets = False }

indent :: OutputInfo a -> OutputInfo a
indent o = o { indentation = 1 + indentation o }

withBrackets :: OutputInfo a -> OutputInfo a
withBrackets o = o { brackets = True }

withoutBrackets :: OutputInfo a -> OutputInfo a
withoutBrackets o = o { brackets = False }

withDeclCommentLine :: (Decl a -> MarkupString Styles) -> OutputInfo a -> OutputInfo a
withDeclCommentLine f o = o { declComment = (\d -> open Comments ++ lift "// " ++ f d ++ close Comments ++ lift "\n") }

join :: (a -> [b]) -> [b] -> [a] -> [b]
join _ _ [] = []
join f _ [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

delim :: (a -> [b]) -> [b] -> [a] -> [b]
delim _ _ [] = []
delim f s (x:xs) = f x ++ s ++ delim f s xs

tabs :: OutputInfo a -> MarkupString Styles
tabs x = lift (take (indentation x) (repeat '\t'))

outputProgram :: OutputInfo a -> Program a -> MarkupString Styles
outputProgram mo (Program pr _)		= join (outputDecl mo) (lift "\n\n") pr

outputDecl :: OutputInfo a -> Decl a -> MarkupString Styles
outputDecl mo decl@(VarDecl t i e _)	= tabs mo ++ (declComment mo decl) ++ tabs mo ++ outputType mo t ++ lift " " ++ variable i ++ lift " = " ++ outputExpr (withoutBrackets mo) e ++ lift ";"
outputDecl mo decl@(FunDecl t i args vdecls stmts _) = tabs mo ++ (declComment mo decl) ++ tabs mo ++ outputType mo t ++ lift " " ++ function i ++ lift "(" ++ join (outputArg mo) (lift ", ") args ++ lift "){\n" ++ delim (outputDecl (indent mo)) (lift "\n") vdecls ++ delim (outputStmt (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"

outputArg :: OutputInfo a -> (Type a, Identifier a) -> MarkupString Styles
outputArg mo (t, i) = outputType mo t ++ lift " " ++ variable i

outputType :: OutputInfo a -> Type a -> MarkupString Styles
outputType mo t = open Type ++ lift (outputType' t) ++ close Type where
	outputType' (Void _)			= "Void"
	outputType' (Int _)			= "Int"
	outputType' (Bool _)			= "Bool"
	outputType' (TypeIdentifier i _)	= getIdentifierString i
	outputType' (Product t1 t2 _)		= "(" ++ erase (outputType mo t1) ++ ", " ++ erase (outputType mo t2) ++ ")"
	outputType' (ListType t _)		= "[" ++ erase (outputType mo t) ++ "]"

outputStmt :: OutputInfo a -> Stmt a -> MarkupString Styles
outputStmt mo (Expr e _)			= tabs mo ++ outputExpr (withoutBrackets mo) e ++ lift ";"
outputStmt mo (Scope [] _)			= tabs mo ++ lift "{}"
outputStmt mo (Scope stmts _)			= tabs mo ++ lift "{\n" ++ delim (outputStmt (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"
outputStmt mo (If e stmt _)			= tabs mo ++ keyword "if" ++ lift "(" ++ outputExpr (withoutBrackets mo) e ++ lift ")" ++ rest mo stmt
outputStmt mo (IfElse e s1 s2 _)		= tabs mo ++ keyword "if" ++ lift "(" ++ outputExpr (withoutBrackets mo) e ++ lift ")" ++ rest mo s1 ++ between ++ rest mo s2
	where between = if isBlock s1
		then keyword " else "
		else lift "\n" ++ tabs mo ++ keyword "else"
outputStmt mo (While e stmt _)			= tabs mo ++ keyword "while" ++ lift "(" ++ outputExpr (withoutBrackets mo) e ++ lift ")" ++ rest mo stmt
outputStmt mo (Assignment i e _)		= tabs mo ++ variable i ++ lift " = " ++ outputExpr (withoutBrackets mo) e ++ lift ";"
outputStmt mo (Return (Just e) _)		= tabs mo ++ keyword "return " ++ outputExpr (withoutBrackets mo) e ++ lift ";"
outputStmt mo (Return Nothing _)		= tabs mo ++ keyword "return" ++ lift ";"

-- Exception for scope after if/else/while
rest :: OutputInfo a -> Stmt a -> MarkupString Styles
rest mo stmt = case stmt of
	(Scope stmts _) -> lift "{\n" ++ delim (outputStmt (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"
	y -> lift "\n"  ++ outputStmt (indent mo) y

isBlock :: Stmt a -> Bool
isBlock (Scope _ _) = True
isBlock _ = False

enclose :: OutputInfo a -> MarkupString Styles -> MarkupString Styles
enclose o str
	| brackets o = lift "(" ++ str ++ lift ")"
	| otherwise  = str

outputExpr :: OutputInfo a -> Expr a -> MarkupString Styles
outputExpr mo (Var i _) 			= variable i
outputExpr mo (Binop e1 bop e2 _)		= enclose mo $ outputExpr (withBrackets mo) e1 ++ outputBinaryOperator mo bop ++ outputExpr (withBrackets mo) e2
outputExpr mo (Unop uop e _)			= enclose mo $ outputUnaryOperator mo uop ++ outputExpr (withBrackets mo) e
outputExpr mo (Kint n _)			= constant (show n)
outputExpr mo (Kbool b _)			= constant (show b)
outputExpr mo (FunCall i exprs _)		= function i ++ lift "(" ++ join (outputExpr (withoutBrackets mo)) (lift ", ") exprs ++ lift ")"
outputExpr mo (Pair e1 e2 _)			= lift "(" ++ outputExpr (withoutBrackets mo) e1 ++ lift ", " ++ outputExpr (withoutBrackets mo) e2 ++ lift ")"
outputExpr mo (Nil _)				= lift "[]"

outputBinaryOperator :: OutputInfo a -> BinaryOperator a -> MarkupString Styles
outputBinaryOperator mo (Multiplication _)	= lift " * "
outputBinaryOperator mo (Division _)		= lift " / "
outputBinaryOperator mo (Modulo _)		= lift " % "
outputBinaryOperator mo (Plus _)		= lift " + "
outputBinaryOperator mo (Minus _)		= lift " - "
outputBinaryOperator mo (Cons _)		= lift " : "
outputBinaryOperator mo (Equals _)		= lift " == "
outputBinaryOperator mo (LesserThan _)		= lift " < "
outputBinaryOperator mo (GreaterThan _)		= lift " > "
outputBinaryOperator mo (LesserEqualThan _)	= lift " <= "
outputBinaryOperator mo (GreaterEqualThan _)	= lift " >= "
outputBinaryOperator mo (Nequals _)		= lift " != "
outputBinaryOperator mo (And _)			= lift " && "
outputBinaryOperator mo (Or _)			= lift " || "

outputUnaryOperator :: OutputInfo a -> UnaryOperator a -> MarkupString Styles
outputUnaryOperator mo (Not _) 			= lift "!"
outputUnaryOperator mo (Negative _)		= lift "-"

outputFreeType :: OutputInfo m -> Typing.FreeType m -> MarkupString Styles
outputFreeType mo (Typing.FT i _)		= open Variable ++ lift "a" ++ close Variable ++ open UniqueID ++ lift (show i) ++ close UniqueID

outputMonoType :: OutputInfo m -> Typing.MonoType m -> MarkupString Styles
outputMonoType mo (Typing.Func args r _)	= (outputMonoType mo r) ++ lift "(" ++ join (outputMonoType mo) (lift ", ") args ++ lift ")"
outputMonoType mo (Typing.Pair x y _)		= lift "(" ++ outputMonoType mo x ++ lift ", " ++ outputMonoType mo y ++ lift ")"
outputMonoType mo (Typing.List t _)		= lift "[" ++ outputMonoType mo t ++ lift "]"
outputMonoType mo (Typing.Free t _)		= outputFreeType mo t
outputMonoType mo (Typing.Int _)		= open Type ++ lift "Int" ++ close Type
outputMonoType mo (Typing.Bool _)		= open Type ++ lift "Bool" ++ close Type
outputMonoType mo (Typing.Void _)		= open Type ++ lift "Void" ++ close Type

outputPolyType :: OutputInfo m -> Typing.PolyType m -> MarkupString Styles
outputPolyType mo (Typing.Poly f t _)		= open Keyword ++ lift "forall " ++ close Keyword ++ outputFreeType mo f ++ lift " . " ++ outputPolyType mo t
outputPolyType mo (Typing.Mono t _)		= outputMonoType mo t
