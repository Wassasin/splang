module Meta where

import qualified Source
import AST

data P1Meta = P1 {src :: Source.IndexSpan}
	deriving (Show, Eq)

type P1 a = a P1Meta

constructP1 :: Source.IndexSpan -> P1Meta
constructP1 l = P1 {src=l}

class ASTMeta a where
	getMeta :: a b -> b

instance ASTMeta Program where
	getMeta (Program m _) = m

instance ASTMeta Decl where
	getMeta (VarDecl m _ _ _) = m
	getMeta (FunDecl m _ _ _ _ _) = m

instance ASTMeta Type where
	getMeta (Void m) = m
	getMeta (Int m) = m
	getMeta (Bool m) = m
	getMeta (Identifier m _) = m
	getMeta (Product m _ _) = m
	getMeta (ListType m _) = m

instance ASTMeta Stmt where
	getMeta (Expr m _) = m
	getMeta (Scope m _) = m
	getMeta (If m _ _) = m
	getMeta (IfElse m _ _ _) = m
	getMeta (While m _ _) = m
	getMeta (Assignment m _ _) = m
	getMeta (Return m _) = m

instance ASTMeta Expr where
	getMeta (Var m _) = m
	getMeta (Binop m _ _ _) = m
	getMeta (Unop m _ _) = m
	getMeta (Kint m _) = m
	getMeta (Kbool m _) = m
	getMeta (FunCall m _ _) = m
	getMeta (Pair m _ _) = m
	getMeta (List m _) = m

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
