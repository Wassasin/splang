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
	(==) (Pair xx xy _) (Pair yx yy _)	= xx == yx && xy == yy
	(==) (List x _) (List y _)		= x == y
	(==) (Free x _) (Free y _)		= x == y
	(==) (Int _) (Int _)			= True
	(==) (Bool _) (Bool _)			= True
	(==) _ _				= False
	
type Substitution m = MonoType m -> MonoType m
data Unification m = Success (Substitution m) | Fail (MonoType m) (MonoType m)

type InferState = FTid
type InferContext m = [(AST.IdentID, PolyType m)]

-- Cannot unify types | IdentID could not be found in context | A substitution of an unbound free variable in a PolyType occurred; probably did not bind the unbound type somewhere
data InferError m = CannotUnify (MonoType m) (MonoType m) | ContextNotFound AST.IdentID | PolyViolation (FreeType m) (MonoType m) | UnknownIdentifier (AST.Identifier m)
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
	
returnInferError :: InferError m -> InferMonadD m a
returnInferError e = mo $ \_ -> returnFatal e

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

apply :: Substitution m -> InferContext m -> InferMonadD m (InferContext m)
apply s c = sequence $ map ( \(i, t) -> do 
		t <- applyPoly s t
		return (i, t)
	) c

applyPoly :: Substitution m -> PolyType m -> InferMonadD m (PolyType m)
applyPoly s (Mono t m) = return $ Mono (s t) m
applyPoly s (Poly a t m) = case s (Free a m) of
	(Free b n)	-> case a == b of
		False	-> returnInferError $ PolyViolation a (Free b n)
		True	-> do
				u <- applyPoly s t
				return (Poly a u m)
	y		-> returnInferError $ PolyViolation a y

fromContext :: InferContext m -> AST.IdentID -> InferMonadD m (PolyType m)
fromContext [] i	= returnInferError $ ContextNotFound i
fromContext ((j,t):cs) i
	| i == j	= return t
	| otherwise	= fromContext cs i

genMgu :: MonoType m -> MonoType m -> InferMonadD m (Substitution m)
genMgu t1 t2 = case mgu t1 t2 of
	Fail u1 u2 -> returnInferError $ CannotUnify u1 u2
	Success s -> return s

genFresh :: m -> InferMonadD m (MonoType m)
genFresh m = mo $ \st -> return (Free (FT st m) m, st+1)

genBind :: m -> PolyType m -> InferMonadD m (MonoType m)
genBind _ (Mono t _) = return t
genBind m (Poly a t _) = do
	b <- genFresh m
	t <- genBind m t
	return $ substitute b (Free a m) t

fetchIdentID :: AST.Identifier m -> InferMonadD m (AST.IdentID)
fetchIdentID (AST.Identifier _ (Just i) _)	= return i
fetchIdentID i					= returnInferError $ UnknownIdentifier i

(.>) :: InferMonadD m (Substitution m) -> Substitution m -> InferMonadD m (Substitution m)
(.>) fd s2 = do
	s1 <- fd
	return $ s1 . s2

matchBinOp :: m -> AST.BinaryOperator m -> InferMonadD m (MonoType m, MonoType m, MonoType m)
matchBinOp m (AST.Multiplication _)	= return (Int m, Int m, Int m)
matchBinOp m (AST.Division _)		= return (Int m, Int m, Int m)
matchBinOp m (AST.Modulo _)		= return (Int m, Int m, Int m)
matchBinOp m (AST.Plus _)		= return (Int m, Int m, Int m)
matchBinOp m (AST.Minus _)		= return (Int m, Int m, Int m)
matchBinOp m (AST.Cons _)		= do
						a <- genFresh m
						return (a, List a m, List a m)
matchBinOp m (AST.Equals _)		= do
						a <- genFresh m
						return (a, a, Bool m)
matchBinOp m (AST.LesserThan _)		= return (Int m, Int m, Bool m)
matchBinOp m (AST.GreaterThan _)	= return (Int m, Int m, Bool m)
matchBinOp m (AST.LesserEqualThan _)	= return (Int m, Int m, Bool m)
matchBinOp m (AST.GreaterEqualThan _)	= return (Int m, Int m, Bool m)
matchBinOp m (AST.Nequals _)		= do
						a <- genFresh m
						return (a, a, Bool m)
matchBinOp m (AST.And _)		= return (Bool m, Bool m, Bool m)
matchBinOp m (AST.Or _)			= return (Bool m, Bool m, Bool m)

matchUnOp :: m -> AST.UnaryOperator m -> InferMonadD m (MonoType m, MonoType m)
matchUnOp m (AST.Not _)		= return (Bool m, Bool m)
matchUnOp m (AST.Negative _)	= return (Int m, Int m)

inferExpr :: InferFunc P2Meta (P2 AST.Expr)
inferExpr c (AST.Var i m) t = do
	i <- fetchIdentID i
	u <- fromContext c i
	u <- genBind m u
	s <- genMgu t u
	return s
inferExpr c (AST.Binop e1 op e2 m) t = do
	(x, y, u) <- matchBinOp m op
	s <- inferExpr c e1 x
	c <- apply s c
	s <- inferExpr c e2 y .> s
	s <- genMgu (s t) u .> s
	return s
inferExpr c (AST.Unop op e m) t = do
	(x, u) <- matchUnOp m op
	s <- inferExpr c e x
	s <- genMgu (s t) u .> s
	return s
inferExpr _ (AST.Kint _ m) t = do
	s <- genMgu (Int m) t
	return s
inferExpr _ (AST.Kbool _ m) t = do
	s <- genMgu (Bool m) t
	return s
inferExpr c (AST.FunCall i es m) t = do
	i <- fetchIdentID i
	u <- fromContext c i
	u <- genBind m u
	let Func as r _ = u
	s <- genMgu t r
	s <- foldl (>>=) (return s) $ map (\(e, a) -> \s -> do
		c <- apply s c
		s <- inferExpr c e (s a) .> s
		return s) $ zip es as
	return s
inferExpr c (AST.Pair e1 e2 m) t = do
	a1 <- genFresh $ getMeta e1
	s <- inferExpr c e1 a1
	c <- apply s c
	a2 <- genFresh $ getMeta e2
	s <- inferExpr c e2 a2 .> s
	s <- genMgu (s t) (s $ Pair a1 a2 m) .> s
	return s
inferExpr _ (AST.Nil m) t = do
	a <- genFresh m
	s <- genMgu t (List a m)
	return s
