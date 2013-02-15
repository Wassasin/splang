module Output (Styles(..), OpenClose(..), Markup, MarkupString, outputProgram) where

import AST

data Styles = Type
	| Variable
	| Constant
	| Keyword

data OpenClose a = Open a | Close a
type Markup a = Either Char (OpenClose a)
type MarkupString a = [Markup a]

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

keyword str = open Keyword ++ lift str ++ close Keyword
constant str = open Constant ++ lift str ++ close Constant
variable str = open Variable ++ lift str ++ close Variable

join :: (a -> [b]) -> [b] -> [a] -> [b]
join f s [] = []
join f s [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

delim :: (a -> [b]) -> [b] -> [a] -> [b]
delim f s [] = []
delim f s (x:xs) = f x ++ s ++ delim f s xs

tabs :: Int -> MarkupString Styles
tabs x = lift (take x (repeat '\t'))

outputProgram :: Program a -> MarkupString Styles
outputProgram (Program pr _) = join (outputDecl 0) (lift "\n\n") pr

outputDecl :: Int -> Decl a -> MarkupString Styles
outputDecl n (VarDecl t i e _) = tabs n ++ outputType t ++ lift " " ++ variable i ++ lift " = " ++ outputExpr e ++ lift ";"
outputDecl n (FunDecl t i args vdecls stmts _) = tabs n ++ outputType t ++ lift " " ++ lift i ++ lift "(" ++ join outputArg (lift ", ") args ++ lift "){\n" ++ delim (outputDecl (n+1)) (lift "\n") vdecls ++ delim (outputStmt (n+1)) (lift "\n") stmts ++ tabs n ++ lift "}"

outputArg :: (Type a, Identifier) -> MarkupString Styles
outputArg (t, i) = outputType t ++ lift " " ++ variable i

outputType :: Type a -> MarkupString Styles
outputType t = open Type ++ lift (outputType' t) ++ close Type where
	outputType' (Void _)			= "Void"
	outputType' (Int _)				= "Int"
	outputType' (Bool _)			= "Bool"
	outputType' (Identifier i _)	= i
	outputType' (Product t1 t2 _)	= "(" ++ erase (outputType t1) ++ ", " ++ erase (outputType t2) ++ ")"
	outputType' (ListType t _)		= "[" ++ erase (outputType t) ++ "]"

outputStmt :: Int -> Stmt a -> MarkupString Styles
outputStmt n (Expr e _)			= tabs n ++ outputExpr e ++ lift ";"
outputStmt n (Scope [] _)		= tabs n ++ lift "{}"
outputStmt n (Scope stmts _)	= tabs n ++ lift "{\n" ++ delim (outputStmt (n+1)) (lift "\n") stmts ++ tabs n ++ lift "}"
outputStmt n (If e stmt _)		= tabs n ++ keyword "if" ++ lift "(" ++ outputExpr e ++ lift ")" ++ rest n stmt
outputStmt n (IfElse e s1 s2 _)	= tabs n ++ keyword "if" ++ lift "(" ++ outputExpr e ++ lift ")" ++ rest n s1 ++ between ++ rest n s2
	where between = if isBlock s1
		then keyword " else "
		else lift "\n" ++ tabs n ++ keyword "else"
outputStmt n (While e stmt _)	= tabs n ++ keyword "while" ++ lift "(" ++ outputExpr e ++ lift ")" ++ rest n stmt
outputStmt n (Assignment i e _)	= tabs n ++ variable i ++ lift " = " ++ outputExpr e ++ lift ";"
outputStmt n (Return (Just e) _)		= tabs n ++ keyword "return " ++ outputExpr e ++ lift ";"
outputStmt n (Return Nothing _)		= tabs n ++ keyword "return" ++ lift ";"

-- Exception for scope after if/else/while
rest :: Int -> Stmt a -> MarkupString Styles
rest n stmt = case stmt of
	(Scope stmts _) -> lift "{\n" ++ delim (outputStmt (n+1)) (lift "\n") stmts ++ tabs n ++ lift "}"
	y -> lift "\n"  ++ outputStmt (n+1) stmt

isBlock :: Stmt a -> Bool
isBlock (Scope _ _) = True
isBlock _ = False

outputExpr :: Expr a -> MarkupString Styles
outputExpr (Var i _) 			= variable i
outputExpr (Binop e1 bop e2 _)	= lift "(" ++ outputExpr e1 ++ outputBinaryOperator bop ++ outputExpr e2 ++ lift ")"
outputExpr (Unop uop e _)		= lift "(" ++ outputUnaryOperator uop ++ outputExpr e ++ lift ")"
outputExpr (Kint n _)			= constant (show n)
outputExpr (Kbool b _)			= constant (show b)
outputExpr (FunCall i exprs _)	= lift i ++ lift "(" ++ join outputExpr (lift ", ") exprs ++ lift ")"
outputExpr (Pair e1 e2 _)		= lift "(" ++ outputExpr e1 ++ lift ", " ++ outputExpr e2 ++ lift ")"
outputExpr (List exprs _)		= lift "[" ++ join outputExpr (lift ", ") exprs ++ lift "]"

outputBinaryOperator :: BinaryOperator a -> MarkupString Styles
outputBinaryOperator (Multiplication _)		= lift " * "
outputBinaryOperator (Division _)			= lift " / "
outputBinaryOperator (Modulo _)				= lift " % "
outputBinaryOperator (Plus _)				= lift " + "
outputBinaryOperator (Minus _)				= lift " - "
outputBinaryOperator (Cons _)				= lift " : "
outputBinaryOperator (Equals _)				= lift " == "
outputBinaryOperator (LesserThan _)			= lift " < "
outputBinaryOperator (GreaterThan _)		= lift " > "
outputBinaryOperator (LesserEqualThan _)	= lift " <= "
outputBinaryOperator (GreaterEqualThan _)	= lift " >= "
outputBinaryOperator (Nequals _)			= lift " != "
outputBinaryOperator (And _)				= lift " && "
outputBinaryOperator (Or _)					= lift " || "

outputUnaryOperator :: UnaryOperator a -> MarkupString Styles
outputUnaryOperator (Not _) 		= lift "!"
outputUnaryOperator (Negative _)	= lift "-"
