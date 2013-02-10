module AST where

-- Derived from the original grammar
type Program = [Decl]

-- I have inlined both VarDecl, FunDecl and FArgs
data Decl = VarDecl Type Identifier Expr
	| FunDecl Type Identifier [(Type, Identifier)] [Decl] [Stmt]
	deriving (Show, Eq)

-- I have merged RetType and Type
data Type = Void
	| Int
	| Bool
	| Identifier Identifier
	| Product Type Type
	| ListType Type
	deriving (Show, Eq)

-- We allow an Expr to be an statement, for example we dont need an extra FunCall now
data Stmt = Expr Expr
	| Scope [Stmt]
	| If Expr Stmt
	| IfElse Expr Stmt Stmt
	| While Expr Stmt
	| Assignment Identifier Expr
	| Return Expr
	deriving (Show, Eq)

data Expr = Var Identifier
	| Binop Expr BinaryOperator Expr
	| Unop UnaryOperator Expr
	| Kint AST.Integer
	| Kbool Boolean
	| FunCall Identifier [Expr]
	| Pair Expr Expr
	| List [Expr]
	deriving (Show, Eq)

data BinaryOperator = Multiplication | Division | Modulo
	| Plus | Minus | Cons
	| Equals | LesserThan | GreaterThan | LesserEqualThan | GreaterEqualThan | Nequals
	| And | Or
	deriving (Show, Eq)

data UnaryOperator = Not | Negative
	deriving (Show, Eq)

type Identifier = String
type Integer = Int
type Boolean = Bool
