module Builtins where

-- Builtin functions
data Builtins
	= Print
	| IsEmpty
	| Head
	| Tail
	| Fst
	| Snd
	deriving (Show, Eq, Read, Enum, Bounded)

isBuiltin :: Int -> Bool
isBuiltin n = n >= (fromEnum (minBound :: Builtins))
	&& n <= (fromEnum (maxBound :: Builtins))
