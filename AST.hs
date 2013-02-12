module AST where

-- Derived from the original grammar
data Program a = Program a [Decl a]

-- I have inlined both VarDecl, FunDecl and FArgs
data Decl a = VarDecl a (Type a) Identifier (Expr a)
	| FunDecl a (Type a) Identifier [(Type a, Identifier)] [Decl a] [Stmt a]
	deriving (Show, Eq)

-- I have merged RetType and Type
data Type a = Void a
	| Int a
	| Bool a
	| Identifier a Identifier
	| Product a (Type a) (Type a)
	| ListType a (Type a)
	deriving (Show, Eq)

-- We allow an Expr to be an statement, for example we dont need an extra FunCall now
data Stmt a = Expr a (Expr a)
	| Scope a [Stmt a]
	| If a (Expr a) (Stmt a)
	| IfElse a (Expr a) (Stmt a) (Stmt a)
	| While a (Expr a) (Stmt a)
	| Assignment a Identifier (Expr a)
	| Return a (Expr a)
	deriving (Show, Eq)

data Expr a = Var a Identifier
	| Binop a (Expr a) (BinaryOperator a) (Expr a)
	| Unop a (UnaryOperator a) (Expr a)
	| Kint a AST.Integer
	| Kbool a Boolean
	| FunCall a Identifier [Expr a]
	| Pair a (Expr a) (Expr a)
	| List a [Expr a]
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
