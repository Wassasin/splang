module Typing where

import Data.List (union)

data FreeType a = FT Int a
	deriving (Show, Read)
	
data PolyType a = Poly (FreeType a) (PolyType a) a | Mono (MonoType a) a
	deriving (Show, Read)

data MonoType a = Func [MonoType a] (MonoType a) a
	| Tuple (MonoType a) (MonoType a) a
	| List (MonoType a) a
	| Free (FreeType a) a
	| Int a
	| Bool a
	deriving (Show, Read)
	
type Substitution a = MonoType a -> MonoType a

data Unification a = Success (Substitution a) | Fail (MonoType a) (MonoType a)

instance Eq (FreeType a) where
	(==) (FT x _) (FT y _) = x == y

instance Eq (MonoType a) where
	(==) (Func xs xr _) (Func ys yr _)	= xs == ys && xr == yr
	(==) (Tuple xx xy _) (Tuple yx yy _)	= xx == yx && yx == yy
	(==) (List x _) (List y _)		= x == y
	(==) (Free x _) (Free y _)		= x == y
	(==) (Int _) (Int _)			= True
	(==) (Bool _) (Bool _)			= True
	(==) _ _				= False

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
ftvm (Tuple t1 t2 _)	= union (ftvm t1) (ftvm t2)
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
mgu (Tuple xx xy _) (Tuple yx yy _)	= case mgu xy yy of
						Success s	-> mgu (s xx) (s yx)
						Fail x y	-> Fail x y
mgu (List x _) (List y _)		= mgu x y
mgu (Int _) (Int _)			= Success id
mgu (Bool _) (Bool _)			= Success id
mgu x y					= Fail x y
