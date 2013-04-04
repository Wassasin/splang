{-# LANGUAGE DeriveFunctor #-}

module Typing where

import Meta (ASTMeta, getMeta)

type FTid = Int
data FreeType m = FT FTid m
	deriving (Show, Read, Functor)
	
data PolyType m = Poly (FreeType m) (PolyType m) m | Mono (MonoType m) m
	deriving (Show, Read, Functor)

data MonoType m = Func [MonoType m] (MonoType m) m
	| Pair (MonoType m) (MonoType m) m
	| List (MonoType m) m
	| Free (FreeType m) m
	| Int m
	| Bool m
	| Void m
	deriving (Show, Read, Functor)

-- Returns whether Void is used in an active position (ie non-return type)
usingVoid :: MonoType m -> Bool
usingVoid (Func args _ _)	= any usingVoid args
usingVoid (Pair t1 t2 _)	= usingVoid t1 || usingVoid t2
usingVoid (List t _)		= usingVoid t
usingVoid (Void _)		= True
usingVoid _			= False
	
instance Eq (FreeType m) where
	(==) (FT x _) (FT y _) = x == y

instance Eq (PolyType m) where
	(==) (Poly ft1 pt1 _) (Poly ft2 pt2 _)	= ft1 == ft2 && pt1 == pt2
	(==) (Mono mt1 _) (Mono mt2 _)		= mt1 == mt2
	(==) _ _				= False

instance Eq (MonoType m) where
	(==) (Func xs xr _) (Func ys yr _)	= xs == ys && xr == yr
	(==) (Pair xx xy _) (Pair yx yy _)	= xx == yx && xy == yy
	(==) (List x _) (List y _)		= x == y
	(==) (Free x _) (Free y _)		= x == y
	(==) (Int _) (Int _)			= True
	(==) (Bool _) (Bool _)			= True
	(==) (Void _) (Void _)			= True
	(==) _ _				= False

instance ASTMeta FreeType where
	getMeta (FT _ m) = m

instance ASTMeta PolyType where
	getMeta (Poly _ _ m) = m
	getMeta (Mono _ m) = m

instance ASTMeta MonoType where
	getMeta (Func _ _ m) = m
	getMeta (Pair _ _ m) = m
	getMeta (List _ m) = m
	getMeta (Free _ m) = m
	getMeta (Int m) = m
	getMeta (Bool m) = m
	getMeta (Void m) = m
