module Meta where

import qualified Source
import AST

data P1Meta = P1 {src :: Source.IndexSpan}
	deriving (Show, Eq, Read)

type P1 a = a P1Meta

constructP1 :: Source.IndexSpan -> P1Meta
constructP1 l = P1 {src=l}

class ASTMeta a where
	getMeta :: a b -> b

instance ASTMeta Program where
	getMeta (Program _ m) = m

instance ASTMeta Decl where
	getMeta (VarDecl _ _ _ m) = m
	getMeta (FunDecl _ _ _ _ _ m) = m

instance ASTMeta Type where
	getMeta (Void m) = m
	getMeta (Int m) = m
	getMeta (Bool m) = m
	getMeta (Identifier _ m) = m
	getMeta (Product _ _ m) = m
	getMeta (ListType _ m) = m

instance ASTMeta Stmt where
	getMeta (Expr _ m) = m
	getMeta (Scope _ m) = m
	getMeta (If _ _ m) = m
	getMeta (IfElse _ _ _ m) = m
	getMeta (While _ _ m) = m
	getMeta (Assignment _ _ m) = m
	getMeta (Return _ m) = m

instance ASTMeta Expr where
	getMeta (Var _ m) = m
	getMeta (Binop _ _ _ m) = m
	getMeta (Unop _ _ m) = m
	getMeta (Kint _ m) = m
	getMeta (Kbool _ m) = m
	getMeta (FunCall _ _ m) = m
	getMeta (Pair _ _ m) = m
	getMeta (List _ m) = m

instance ASTMeta BinaryOperator where
	getMeta (Multiplication m) = m
	getMeta (Division m) = m
	getMeta (Modulo m) = m
	getMeta (Plus m) = m
	getMeta (Minus m) = m
	getMeta (Cons m) = m
	getMeta (Equals m) = m
	getMeta (LesserThan m) = m
	getMeta (GreaterThan m) = m
	getMeta (LesserEqualThan m) = m
	getMeta (GreaterEqualThan m) = m
	getMeta (Nequals m) = m
	getMeta (And m) = m
	getMeta (Or m) = m

instance ASTMeta UnaryOperator where
	getMeta (Not m) = m
	getMeta (Negative m) = m