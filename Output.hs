module Output where

import AST

join :: (a -> String) -> String -> [a] -> String
join f s [] = ""
join f s [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

tabs :: Int -> String
tabs x = take x (repeat '\t')

outputProgram :: Program a -> String
outputProgram (Program _ xs) = join (outputDecl 0) "\n\n" xs

outputDecl :: Int -> Decl a -> String
outputDecl n (VarDecl _ t i e) = tabs n ++ outputType t ++ " " ++ i ++ " = " ++ outputExpr e ++ ";"
outputDecl n (FunDecl _ t i args vdecls stmts) = tabs n ++ outputType t ++ " " ++ i ++ "(" ++ join outputArg ", " args ++ "){\n" ++ join (outputDecl (n+1)) "\n" vdecls ++ "\n" ++ join (outputStmt (n+1)) "\n" stmts ++ tabs n ++ "\n}"

outputArg :: (Type a, Identifier) -> String
outputArg (t, i) = outputType t ++ " " ++ i

outputType :: Type a -> String
outputType (Void _)		= "Void"
outputType (Int _)		= "Int"
outputType (Bool _)		= "Bool"
outputType (Identifier _ i)	= i
outputType (Product _ t1 t2)	= "(" ++ outputType t1 ++ ", " ++ outputType t2 ++ ")"
outputType (ListType _ t)	= "[" ++ outputType t ++ "]"

outputStmt :: Int -> Stmt a -> String
outputStmt n (Expr _ e)		= tabs n ++ outputExpr e ++ ";"
outputStmt n (Scope _ stmts)	= tabs n ++ "{\n" ++ join (outputStmt (n+1)) "\n" stmts ++ "\n" ++ tabs n ++ "}"
outputStmt n (If _ e stmt)	= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) stmt
outputStmt n (IfElse _ e s1 s2)	= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) s1 ++ "\n" ++ tabs n ++ "else\n" ++ outputStmt (n+1) s2
outputStmt n (While _ e stmt)	= tabs n ++ "while(" ++ outputExpr e ++ ")\n" ++ tabs n ++ outputStmt (n+1) stmt
outputStmt n (Assignment _ i e)	= tabs n ++ i ++ " = " ++ outputExpr e ++ ";"
outputStmt n (Return _ e)	= tabs n ++ "return " ++ outputExpr e ++ ";"

outputExpr :: Expr a -> String
outputExpr (Var _ i) 		= i
outputExpr (Binop _ e1 bop e2)	= "(" ++ outputExpr e1 ++ outputBinaryOperator bop ++ outputExpr e2 ++ ")"
outputExpr (Unop _ uop e)	= "(" ++ outputUnaryOperator uop ++ outputExpr e ++ ")"
outputExpr (Kint _ n)		= show n
outputExpr (Kbool _ b)		= show b
outputExpr (FunCall _ i exprs)	= i ++ "(" ++ join outputExpr ", " exprs ++ ")"
outputExpr (Pair _ e1 e2)	= "(" ++ outputExpr e1 ++ ", " ++ outputExpr e2 ++ ")"
outputExpr (List _ exprs)	= "[" ++ join outputExpr ", " exprs ++ "]"

outputBinaryOperator :: BinaryOperator a -> String
outputBinaryOperator (Multiplication _)		= " * "
outputBinaryOperator (Division _)		= " / "
outputBinaryOperator (Modulo _)			= " % "
outputBinaryOperator (Plus _)			= " + "
outputBinaryOperator (Minus _)			= " - "
outputBinaryOperator (Cons _)			= " : "
outputBinaryOperator (Equals _)			= " == "
outputBinaryOperator (LesserThan _)		= " < "
outputBinaryOperator (GreaterThan _)		= " > "
outputBinaryOperator (LesserEqualThan _)	= " <= "
outputBinaryOperator (GreaterEqualThan _)	= " >= "
outputBinaryOperator (Nequals _)		= " != "
outputBinaryOperator (And _)			= " && "
outputBinaryOperator (Or _)			= " || "

outputUnaryOperator :: UnaryOperator a -> String
outputUnaryOperator (Not _)		= "!"
outputUnaryOperator (Negative _)	= "-"

