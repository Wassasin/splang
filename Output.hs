module Output where

import AST

join :: (a -> String) -> String -> [a] -> String
join f s [] = ""
join f s [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

tabs :: Int -> String
tabs x = take x (repeat '\t')

outputProgram :: Program -> String
outputProgram pr = join (outputDecl 0) "\n\n" pr

outputDecl :: Int -> Decl -> String
outputDecl n (VarDecl t i e) = tabs n ++ outputType t ++ " " ++ i ++ " = " ++ outputExpr e ++ ";"
outputDecl n (FunDecl t i args vdecls stmts) = tabs n ++ outputType t ++ " " ++ i ++ "(" ++ join outputArg ", " args ++ "){\n" ++ join (outputDecl (n+1)) "\n" vdecls ++ "\n" ++ join (outputStmt (n+1)) "\n" stmts ++ tabs n ++ "\n}"

outputArg :: (Type, Identifier) -> String
outputArg (t, i) = outputType t ++ " " ++ i

outputType :: Type -> String
outputType Void				= "Void"
outputType Int				= "Int"
outputType Bool				= "Bool"
outputType (Identifier i)	= i
outputType (Product t1 t2)	= "(" ++ outputType t1 ++ ", " ++ outputType t2 ++ ")"
outputType (ListType t)		= "[" ++ outputType t ++ "]"

outputStmt :: Int -> Stmt -> String
outputStmt n (Expr e)			= tabs n ++ outputExpr e ++ ";"
outputStmt n (Scope stmts)		= tabs n ++ "{\n" ++ join (outputStmt (n+1)) "\n" stmts ++ "\n" ++ tabs n ++ "}"
outputStmt n (If e stmt)		= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) stmt
outputStmt n (IfElse e s1 s2)	= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) s1 ++ "\n" ++ tabs n ++ "else\n" ++ outputStmt (n+1) s2
outputStmt n (While e stmt)		= tabs n ++ "while(" ++ outputExpr e ++ ")\n" ++ tabs n ++ outputStmt (n+1) stmt
outputStmt n (Assignment i e)	= tabs n ++ i ++ " = " ++ outputExpr e ++ ";"
outputStmt n (Return e)			= tabs n ++ "return " ++ outputExpr e ++ ";"

outputExpr :: Expr -> String
outputExpr (Var i) 				= i
outputExpr (Binop e1 bop e2)	= "(" ++ outputExpr e1 ++ outputBinaryOperator bop ++ outputExpr e2 ++ ")"
outputExpr (Unop uop e)			= "(" ++ outputUnaryOperator uop ++ outputExpr e ++ ")"
outputExpr (Kint n)				= show n
outputExpr (Kbool b)			= show b
outputExpr (FunCall i exprs)	= i ++ "(" ++ join outputExpr ", " exprs ++ ")"
outputExpr (Pair e1 e2)			= "(" ++ outputExpr e1 ++ ", " ++ outputExpr e2 ++ ")"
outputExpr (List exprs)			= "[" ++ join outputExpr ", " exprs ++ "]"

outputBinaryOperator :: BinaryOperator -> String
outputBinaryOperator Multiplication		= " * "
outputBinaryOperator Division			= " / "
outputBinaryOperator Modulo				= " % "
outputBinaryOperator Plus				= " + "
outputBinaryOperator Minus				= " - "
outputBinaryOperator Cons				= " : "
outputBinaryOperator Equals				= " == "
outputBinaryOperator LesserThan			= " < "
outputBinaryOperator GreaterThan		= " > "
outputBinaryOperator LesserEqualThan	= " <= "
outputBinaryOperator GreaterEqualThan	= " >= "
outputBinaryOperator Nequals			= " != "
outputBinaryOperator And				= " && "
outputBinaryOperator Or					= " || "

outputUnaryOperator :: UnaryOperator -> String
outputUnaryOperator Not = "!"
outputUnaryOperator Negative = "-"

