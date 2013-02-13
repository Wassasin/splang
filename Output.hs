module Output where

import AST

join :: (a -> String) -> String -> [a] -> String
join f s [] = ""
join f s [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

tabs :: Int -> String
tabs x = take x (repeat '\t')

outputProgram :: Program a -> String
outputProgram (Program xs _) = join (outputDecl 0) "\n\n" xs

outputDecl :: Int -> Decl a -> String
outputDecl n (VarDecl t i e _) = tabs n ++ outputType t ++ " " ++ i ++ " = " ++ outputExpr e ++ ";"
outputDecl n (FunDecl t i args vdecls stmts _) = tabs n ++ outputType t ++ " " ++ i ++ "(" ++ join outputArg ", " args ++ "){\n" ++ join (outputDecl (n+1)) "\n" vdecls ++ "\n" ++ join (outputStmt (n+1)) "\n" stmts ++ tabs n ++ "\n}"

outputArg :: (Type a, Identifier) -> String
outputArg (t, i) = outputType t ++ " " ++ i

outputType :: Type a -> String
outputType (Void _)		= "Void"
outputType (Int _)		= "Int"
outputType (Bool _)		= "Bool"
outputType (Identifier i _)	= i
outputType (Product t1 t2 _)	= "(" ++ outputType t1 ++ ", " ++ outputType t2 ++ ")"
outputType (ListType t _)	= "[" ++ outputType t ++ "]"

outputStmt :: Int -> Stmt a -> String
outputStmt n (Expr e _)		= tabs n ++ outputExpr e ++ ";"
outputStmt n (Scope stmts _)	= tabs n ++ "{\n" ++ join (outputStmt (n+1)) "\n" stmts ++ "\n" ++ tabs n ++ "}"
outputStmt n (If e stmt _)	= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) stmt
outputStmt n (IfElse e s1 s2 _)	= tabs n ++ "if(" ++ outputExpr e ++ ")\n" ++ outputStmt (n+1) s1 ++ "\n" ++ tabs n ++ "else\n" ++ outputStmt (n+1) s2
outputStmt n (While e stmt _)	= tabs n ++ "while(" ++ outputExpr e ++ ")\n" ++ tabs n ++ outputStmt (n+1) stmt
outputStmt n (Assignment i e _)	= tabs n ++ i ++ " = " ++ outputExpr e ++ ";"
outputStmt n (Return e _)	= tabs n ++ "return " ++ outputExpr e ++ ";"

outputExpr :: Expr a -> String
outputExpr (Var i _) 		= i
outputExpr (Binop e1 bop e2 _)	= "(" ++ outputExpr e1 ++ outputBinaryOperator bop ++ outputExpr e2 ++ ")"
outputExpr (Unop uop e _)	= "(" ++ outputUnaryOperator uop ++ outputExpr e ++ ")"
outputExpr (Kint n _)		= show n
outputExpr (Kbool b _)		= show b
outputExpr (FunCall i exprs _)	= i ++ "(" ++ join outputExpr ", " exprs ++ ")"
outputExpr (Pair e1 e2 _)	= "(" ++ outputExpr e1 ++ ", " ++ outputExpr e2 ++ ")"
outputExpr (List exprs _)	= "[" ++ join outputExpr ", " exprs ++ "]"

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

