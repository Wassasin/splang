module AST where

-- Derived from the original grammar
data Program a = Program [Decl a] a

-- I have inlined both VarDecl, FunDecl and FArgs
data Decl a = VarDecl (Type a) Identifier (Expr a) a
	| FunDecl (Type a) Identifier [(Type a, Identifier)] [Decl a] [Stmt a] a
	deriving (Show, Eq)

-- I have merged RetType and Type
data Type a = Void a
	| Int a
	| Bool a
	| Identifier Identifier a
	| Product (Type a) (Type a) a
	| ListType (Type a) a
	deriving (Show, Eq)

-- We allow an Expr to be an statement, for example we dont need an extra FunCall now
data Stmt a = Expr (Expr a) a
	| Scope [Stmt a] a
	| If (Expr a) (Stmt a) a
	| IfElse (Expr a) (Stmt a) (Stmt a) a
	| While (Expr a) (Stmt a) a
	| Assignment Identifier (Expr a) a
	| Return (Expr a) a
	deriving (Show, Eq)

data Expr a = Var Identifier a
	| Binop (Expr a) (BinaryOperator a) (Expr a) a
	| Unop (UnaryOperator a) (Expr a) a
	| Kint AST.Integer a
	| Kbool Boolean a
	| FunCall Identifier [Expr a] a
	| Pair (Expr a) (Expr a) a
	| List [Expr a] a
	deriving (Show, Eq)

data BinaryOperator a = Multiplication a | Division a | Modulo a
	| Plus a | Minus a | Cons a
	| Equals a | LesserThan a | GreaterThan a | LesserEqualThan a | GreaterEqualThan a | Nequals a
	| And a | Or a
	deriving (Show, Eq)

data UnaryOperator a = Not a | Negative a
	deriving (Show, Eq)

type Identifier = String
type Integer = Int
type Boolean = Bool
