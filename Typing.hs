module Typing where

import Data.List (union)
import SemanticAnalysis (P2, P2Meta)
import Errors
import Meta (ASTMeta, getMeta)
import qualified AST

type FTid = Int
data FreeType m = FT FTid m
	deriving (Show, Read)
	
data PolyType m = Poly (FreeType m) (PolyType m) m | Mono (MonoType m) m
	deriving (Show, Read)

data MonoType m = Func [MonoType m] (MonoType m) m
	| Pair (MonoType m) (MonoType m) m
	| List (MonoType m) m
	| Free (FreeType m) m
	| Int m
	| Bool m
	deriving (Show, Read)
	
instance Eq (FreeType m) where
	(==) (FT x _) (FT y _) = x == y

instance Eq (MonoType m) where
	(==) (Func xs xr _) (Func ys yr _)	= xs == ys && xr == yr
	(==) (Pair xx xy _) (Pair yx yy _)	= xx == yx && yx == yy
	(==) (List x _) (List y _)		= x == y
	(==) (Free x _) (Free y _)		= x == y
	(==) (Int _) (Int _)			= True
	(==) (Bool _) (Bool _)			= True
	(==) _ _				= False
	
type Substitution m = MonoType m -> MonoType m
data Unification m = Success (Substitution m) | Fail (MonoType m) (MonoType m)

type InferState = FTid
type InferContext m = [(AST.IdentID, MonoType m)]

data InferError m = CannotUnify (MonoType m) (MonoType m)
data InferWarning m = Void
type InferResult m a = ErrorContainer (InferError m) (InferWarning m) (a, InferState)

type InferMonad m a = InferState -> InferResult m a
data InferMonadD m a = IM (InferMonad m a)
type InferFunc m a = InferContext m -> a -> MonoType m -> InferMonadD m (Substitution m)

bo :: InferMonadD m a -> InferMonad m a
bo (IM f) = f

mo :: InferMonad m a -> InferMonadD m a
mo f = IM f

instance Monad (InferMonadD m) where
	-- (>>=) :: InferMonadD m a -> (InferResult m a -> InferMonadD m b) -> InferMonadD m b
	(>>=) fd gm = mo $ \st1 -> do
		(a, st2) <- bo fd st1
		(b, st3) <- bo (gm a) st2
		return (b, st3)
	
	-- return :: a -> InferMonadD m a
	return a = mo $ \st -> return (a, st) 

substitute :: MonoType m -> MonoType m -> Substitution m
substitute x y z
	| x == z	= y
	| otherwise	= z

compose :: Unification m -> Unification m -> Unification m
compose (Success x) (Success y)	= Success (x . y)
compose (Fail x y) _		= Fail x y
compose _ (Fail x y)		= Fail x y

ftvm :: MonoType m -> [FreeType m]
ftvm (Free a _)		= [a]
ftvm (Func ts t _)	= foldr (union . ftvm) (ftvm t) ts
ftvm (Pair t1 t2 _)	= union (ftvm t1) (ftvm t2)
ftvm (List t _)		= ftvm t
ftvm _			= []

ftv :: PolyType m -> [FreeType m]
ftv (Mono t _) = ftvm t
ftv (Poly a t _) = filter ((/=) a) (ftv t)

mgu :: MonoType m -> MonoType m -> Unification m
mgu (Free _ _) (Free _ _)		= Success id
mgu (Free a al) t
	| elem a (ftvm t)		= Fail (Free a al) t
	| otherwise			= Success (substitute (Free a al) t)
mgu t (Free b bl)			= mgu (Free b bl) t
mgu (Func xs xr _) (Func ys yr _)	= foldr (\(x, y) su -> case su of
							Success s	-> compose (mgu (s x) (s y)) su
							Fail x y	-> Fail x y
						) (mgu xr yr) (zip xs ys)
mgu (Pair xx xy _) (Pair yx yy _)	= case mgu xy yy of
						Success s	-> mgu (s xx) (s yx)
						Fail x y	-> Fail x y
mgu (List x _) (List y _)		= mgu x y
mgu (Int _) (Int _)			= Success id
mgu (Bool _) (Bool _)			= Success id
mgu x y					= Fail x y

apply :: Substitution m -> InferContext m -> InferContext m
apply s c = map (\(i, t) -> (i, s t)) c

genMgu :: MonoType m -> MonoType m -> InferMonadD m (Substitution m)
genMgu t1 t2 = mo $ \st -> case mgu t1 t2 of
	Fail u1 u2 -> returnFatal $ CannotUnify u1 u2
	Success s -> return (s, st)

genFresh :: m -> InferMonadD m (MonoType m)
genFresh m = mo $ \st -> return (Free (FT st m) m, st+1)

(.>) :: InferMonadD m (Substitution m) -> Substitution m -> InferMonadD m (Substitution m)
(.>) fd s2 = do
	s1 <- fd
	return $ s1 . s2

matchOp :: m -> AST.BinaryOperator m -> InferMonadD m (MonoType m, MonoType m, MonoType m)
matchOp m (AST.Multiplication _)	= return (Int m, Int m, Int m)
matchOp m (AST.Division _)		= return (Int m, Int m, Int m)
matchOp m (AST.Modulo _)		= return (Int m, Int m, Int m)
matchOp m (AST.Plus _)			= return (Int m, Int m, Int m)
matchOp m (AST.Minus _)			= return (Int m, Int m, Int m)
matchOp m (AST.Cons _)			= do
						a <- genFresh m
						return (a, List a m, List a m)
matchOp m (AST.Equals _)		= do
						a <- genFresh m
						return (a, a, Bool m)
matchOp m (AST.LesserThan _)		= return (Int m, Int m, Bool m)
matchOp m (AST.GreaterThan _)		= return (Int m, Int m, Bool m)
matchOp m (AST.LesserEqualThan _)	= return (Int m, Int m, Bool m)
matchOp m (AST.GreaterEqualThan _)	= return (Int m, Int m, Bool m)
matchOp m (AST.Nequals _)		= do
						a <- genFresh m
						return (a, a, Bool m)
matchOp m (AST.And _)			= return (Bool m, Bool m, Bool m)
matchOp m (AST.Or _)			= return (Bool m, Bool m, Bool m)

inferExpr :: InferFunc P2Meta (P2 AST.Expr)
inferExpr c (AST.Binop e1 op e2 m) t = do
	(s1, s2, u) <- matchOp m op
	s <- inferExpr c e1 s1
	s <- inferExpr (apply s c) e2 s2 .> s
	s <- genMgu (s t) u .> s
	return s
inferExpr c (AST.Pair e1 e2 m) t = do
	a1 <- genFresh $ getMeta e1
	s <- inferExpr c e1 a1
	a2 <- genFresh $ getMeta e2
	s <- inferExpr (apply s c) e2 a2 .> s
	s <- genMgu (s t) (s $ Pair a1 a2 m) .> s
	return s
