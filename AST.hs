{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module AST where

-- cabal install derive
import Data.DeriveTH

-- Deriving functor makes it possible to transform Program a into Program b whenever we have a function a -> b
-- For example we can do (fmap (const ()) program) to get rid of all meta-information

-- Derived from the original grammar
data Program a = Program [Decl a] a
	deriving (Show, Eq, Read, Functor)

data ExternLanguage a = ExternLanguage String a
	deriving (Show, Eq, Read, Functor)

data FunctionAttributes = Export
	deriving (Show, Eq, Read)

-- I have inlined both VarDecl, FunDecl and FArgs
data Decl a = VarDecl (Type a) (Identifier a) (Expr a) a
	| FunDecl (Type a) (Identifier a) [(Type a, Identifier a)] [Decl a] [Stmt a] [FunctionAttributes] a
	| ExternDecl (ExternLanguage a) (Type a) (Identifier a) [(Type a, Identifier a)] a
	deriving (Show, Eq, Read, Functor)

getIdentifier :: Decl a -> Identifier a
getIdentifier (VarDecl _ i _ _) = i
getIdentifier (FunDecl _ i _ _ _ _ _) = i
getIdentifier (ExternDecl _ _ i _ _) = i

-- I have merged RetType and Type
data Type a = Void a
	| Int a
	| Bool a
	| TypeIdentifier (Identifier a) a
	| Product (Type a) (Type a) a
	| ListType (Type a) a
	deriving (Show, Eq, Read, Functor)

-- We allow an Expr to be an statement, for example we dont need an extra FunCall now
data Stmt a = Expr (Expr a) a
	| Scope [Stmt a] a
	| If (Expr a) (Stmt a) a
	| IfElse (Expr a) (Stmt a) (Stmt a) a
	| While (Expr a) (Stmt a) a
	| Assignment (Identifier a) (Expr a) a
	| Return (Maybe (Expr a)) a
	deriving (Show, Eq, Read, Functor)

data Expr a = Var (Identifier a) a
	| Binop (Expr a) (BinaryOperator a) (Expr a) a
	| Unop (UnaryOperator a) (Expr a) a
	| Kint AST.Integer a
	| Kbool Boolean a
	| FunCall (Identifier a) [Expr a] a
	| Pair (Expr a) (Expr a) a
	| Nil a
	deriving (Show, Eq, Read, Functor)

data BinaryOperator a = Multiplication a | Division a | Modulo a
	| Plus a | Minus a | Cons a
	| Equals a | LesserThan a | GreaterThan a | LesserEqualThan a | GreaterEqualThan a | Nequals a
	| And a | Or a
	deriving (Show, Eq, Ord, Read, Functor)

data UnaryOperator a = Not a | Negative a
	deriving (Show, Eq, Ord, Read, Functor)

type IdentID = Int
data IdentInfo a = IdentInfo { externLanguage :: ExternLanguage a }
	deriving (Show, Eq, Read, Functor)
data Identifier a = Identifier String (Maybe IdentID) (IdentInfo a) a
	deriving (Show, Eq, Read, Functor)

setIdentInfo :: IdentInfo a -> Identifier a -> Identifier a
setIdentInfo i (Identifier s n _ m) = Identifier s n i m

assignUniqueID :: Identifier a -> IdentID -> Identifier a
assignUniqueID (Identifier str _ l m) n = (Identifier str (Just n) l m)

getIdentifierString :: Identifier a -> String
getIdentifierString (Identifier str _ _ _) = str

type Integer = Int
type Boolean = Bool

-- Derive the isVarDecl functions
$( derive makeIs ''Decl)
$( derive makeIs ''Type)
$( derive makeIs ''Stmt)
$( derive makeIs ''Expr)
