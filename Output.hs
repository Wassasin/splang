{-# LANGUAGE TypeSynonymInstances #-}

module Output where

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

tabs :: Int -> MarkupString Styles
tabs x = lift (take x (repeat '\t'))

outputProgram :: Program -> MarkupString Styles
outputProgram pr = join (outputDecl 0) (lift "\n\n") pr

outputDecl :: Int -> Decl -> MarkupString Styles
outputDecl n (VarDecl t i e) = tabs n ++ outputType t ++ lift " " ++ variable i ++ lift " = " ++ outputExpr e ++ lift ";"
outputDecl n (FunDecl t i args vdecls stmts) = tabs n ++ outputType t ++ lift " " ++ lift i ++ lift "(" ++ join outputArg (lift ", ") args ++ lift "){\n" ++ join (outputDecl (n+1)) (lift "\n") vdecls ++ lift "\n" ++ join (outputStmt (n+1)) (lift "\n") stmts ++ tabs n ++ lift "\n}"

outputArg :: (Type, Identifier) -> MarkupString Styles
outputArg (t, i) = outputType t ++ lift " " ++ variable i

outputType :: Type -> MarkupString Styles
outputType t = open Type ++ lift (outputType' t) ++ close Type where
	outputType' Void			= "Void"
	outputType' Int				= "Int"
	outputType' Bool			= "Bool"
	outputType' (Identifier i)	= i
	outputType' (Product t1 t2)	= "(" ++ erase (outputType t1) ++ ", " ++ erase (outputType t2) ++ ")"
	outputType' (ListType t)	= "[" ++ erase (outputType t) ++ "]"

outputStmt :: Int -> Stmt -> MarkupString Styles
outputStmt n (Expr e)			= tabs n ++ outputExpr e ++ lift ";"
outputStmt n (Scope stmts)		= tabs n ++ lift "{\n" ++ join (outputStmt (n+1)) (lift "\n") stmts ++ lift "\n" ++ tabs n ++ lift "}"
outputStmt n (If e stmt)		= tabs n ++ keyword "if" ++ lift "(" ++ outputExpr e ++ lift ")\n" ++ outputStmt (n+1) stmt
outputStmt n (IfElse e s1 s2)	= tabs n ++ keyword "if" ++ lift "(" ++ outputExpr e ++ lift ")\n" ++ outputStmt (n+1) s1 ++ lift "\n" ++ tabs n ++ keyword "else" ++ lift "\n" ++ outputStmt (n+1) s2
outputStmt n (While e stmt)		= tabs n ++ keyword "while(" ++ outputExpr e ++ lift ")\n" ++ tabs n ++ outputStmt (n+1) stmt
outputStmt n (Assignment i e)	= tabs n ++ variable i ++ lift " = " ++ outputExpr e ++ lift ";"
outputStmt n (Return e)			= tabs n ++ keyword "return " ++ outputExpr e ++ lift ";"

outputExpr :: Expr -> MarkupString Styles
outputExpr (Var i) 				= variable i
outputExpr (Binop e1 bop e2)	= lift "(" ++ outputExpr e1 ++ outputBinaryOperator bop ++ outputExpr e2 ++ lift ")"
outputExpr (Unop uop e)			= lift "(" ++ outputUnaryOperator uop ++ outputExpr e ++ lift ")"
outputExpr (Kint n)				= constant (show n)
outputExpr (Kbool b)			= constant (show b)
outputExpr (FunCall i exprs)	= lift i ++ lift "(" ++ join outputExpr (lift ", ") exprs ++ lift ")"
outputExpr (Pair e1 e2)			= lift "(" ++ outputExpr e1 ++ lift ", " ++ outputExpr e2 ++ lift ")"
outputExpr (List exprs)			= lift "[" ++ join outputExpr (lift ", ") exprs ++ lift "]"

outputBinaryOperator :: BinaryOperator -> MarkupString Styles
outputBinaryOperator Multiplication		= lift " * "
outputBinaryOperator Division			= lift " / "
outputBinaryOperator Modulo				= lift " % "
outputBinaryOperator Plus				= lift " + "
outputBinaryOperator Minus				= lift " - "
outputBinaryOperator Cons				= lift " : "
outputBinaryOperator Equals				= lift " == "
outputBinaryOperator LesserThan			= lift " < "
outputBinaryOperator GreaterThan		= lift " > "
outputBinaryOperator LesserEqualThan	= lift " <= "
outputBinaryOperator GreaterEqualThan	= lift " >= "
outputBinaryOperator Nequals			= lift " != "
outputBinaryOperator And				= lift " && "
outputBinaryOperator Or					= lift " || "

outputUnaryOperator :: UnaryOperator -> MarkupString Styles
outputUnaryOperator Not 		= lift "!"
outputUnaryOperator Negative	= lift "-"
