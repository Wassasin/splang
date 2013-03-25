module Typing where

import Data.List (union)
import SemanticAnalysis (P2, P2Meta)
import Errors
import Meta (ASTMeta, getMeta)
import qualified AST

type FTid = Int
data FreeType a = FT FTid a
	deriving (Show, Read)
	
data PolyType a = Poly (FreeType a) (PolyType a) a | Mono (MonoType a) a
	deriving (Show, Read)

data MonoType a = Func [MonoType a] (MonoType a) a
	| Pair (MonoType a) (MonoType a) a
	| List (MonoType a) a
	| Free (FreeType a) a
	| Int a
	| Bool a
	deriving (Show, Read)
	
instance Eq (FreeType a) where
	(==) (FT x _) (FT y _) = x == y

instance Eq (MonoType a) where
	(==) (Func xs xr _) (Func ys yr _)	= xs == ys && xr == yr
	(==) (Pair xx xy _) (Pair yx yy _)	= xx == yx && yx == yy
	(==) (List x _) (List y _)		= x == y
	(==) (Free x _) (Free y _)		= x == y
	(==) (Int _) (Int _)			= True
	(==) (Bool _) (Bool _)			= True
	(==) _ _				= False
	
type Substitution a = MonoType a -> MonoType a

data Unification a = Success (Substitution a) | Fail (MonoType a) (MonoType a)

type InferState = FTid
type InferContext a = [(AST.IdentID, MonoType a)]

data InferError a = CannotUnify (MonoType a) (MonoType a)
data InferWarning a = Void
type InferResult a b = ErrorContainer (InferError b) (InferWarning b) (a, InferState)

type InferMonad a b = InferState -> InferResult a b
type InferFunc a b = InferContext b -> a -> MonoType b -> InferMonad (Substitution b) b

substitute :: MonoType a -> MonoType a -> Substitution a
substitute x y z
	| x == z	= y
	| otherwise	= z

compose :: Unification a -> Unification a -> Unification a
compose (Success x) (Success y)	= Success (x . y)
compose (Fail x y) _		= Fail x y
compose _ (Fail x y)		= Fail x y

ftvm :: MonoType a -> [FreeType a]
ftvm (Free a _)		= [a]
ftvm (Func ts t _)	= foldr (union . ftvm) (ftvm t) ts
ftvm (Pair t1 t2 _)	= union (ftvm t1) (ftvm t2)
ftvm (List t _)		= ftvm t
ftvm _			= []

ftv :: PolyType a -> [FreeType a]
ftv (Mono t _) = ftvm t
ftv (Poly a t _) = filter ((/=) a) (ftv t)

mgu :: MonoType a -> MonoType a -> Unification a
mgu (Free _ _) (Free _ _)		= Success id
mgu (Free a al) t
	| elem a (ftvm t)		= Fail (Free a al) t
	| otherwise			= Success (substitute (Free a al) t)
mgu t (Free b bl)			= mgu (Free b bl) t
mgu (Func xs xr _) (Func ys yr _)	= foldr (\ (x, y) su -> case su of
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

apply :: InferContext a -> Substitution a -> InferContext a
apply c s = map (\(i, t) -> (i, s t)) c

genMgu :: MonoType a -> MonoType a -> InferMonad (Substitution a) a
genMgu t1 t2 = \st -> case mgu t1 t2 of
	Fail u1 u2 -> returnFatal $ CannotUnify u1 u2
	Success s -> return (s, st)

genFresh :: ASTMeta a => a b -> InferMonad (MonoType b) b
genFresh x = let m = getMeta x in \st -> return (Free (FT st m) m, st+1)

inferExpr :: InferFunc (P2 AST.Expr) P2Meta
inferExpr c t (Pair e1 e2 m) = do
	a1 <- genFresh (getMeta e1)
	s1 <- inferExpr c e1 a1
	a2 <- genFresh
	s2 <- inferExpr (apply c s1) e2 a2
	let s3 = s2 . s1
	s4 <- genMgu (s3 t) (s3 $ Pair a1 a2)
	return $ s4 . s3
