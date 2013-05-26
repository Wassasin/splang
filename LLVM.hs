{-# LANGUAGE GADTs, DataKinds, EmptyDataDecls, KindSignatures, ScopedTypeVariables, DeriveDataTypeable #-}

module LLVM where --(Type(..), Binop(..), Temporary(..), Value(Const, Binop, Load, Store), Instruction(..), Function(..), mkTemp, Program, showProgram) where

import Data.Data
import Data.List
import Data.Char


-- TODO: Calls, Builtins, Structs, Globals, Moves, ...


-- Types we have
-- Simplifications: only integral types (no floats), only signed integers
data Type where
	Array :: Int -> Type -> Type		-- [<# elements> x <elementtype>]
	FunctionType :: Type -> [Type] -> Type	-- <returntype> (<parameter list>)
	IntegralType :: Int -> Type		-- iN
	Void :: Type				-- void
	Struct :: [Type] -> Type		-- type { <type list> } (non-packed)
	Pointer :: Type -> Type
	deriving (Eq)

instance Show Type where
	show (Array n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
	show (FunctionType r l) = show r ++ "(" ++ concat (intersperse "," (map show l)) ++ ")"
	show (IntegralType n) = "i" ++ show n
	show (Void) = "void"
	show (Struct l) = "{" ++ concat (intersperse "," (map show l)) ++ "}"
	show (Pointer t) = show t ++ "*"

i32 :: Type -- The default Integer type
i32 = IntegralType 32

i1 :: Type -- The Bool type
i1 = IntegralType 1


-- Binary operators only work on integral (and vector) types
-- Logical operators are bitwise. Left out: shifts, xor, ...?
data Binop = Add | Sub | Mul | Div | Rem | And | Or
	deriving (Eq, Typeable, Data)

instance Show Binop where
	show = map toLower . showConstr . toConstr


-- Variables
newtype Temporary = T Int
instance Show Temporary where
	show (T n) = "%" ++ show n


-- Expressions
-- Note: we cannot directly make Temporary type-correct, use mkTemp for that
data Value (t :: Type) where
	Temporary	:: Temporary -> Type -> Value t			-- %t
	Const		:: Int -> Value i32				-- i32 <n>
	Binop		:: Binop -> Value (IntegralType n) -> Value (IntegralType n) -> Value (IntegralType n) -- <binop> <ty> <op1>, <op2>
	Load		:: Value (Pointer t) -> Value t			--
	Store		:: Value t -> Value (Pointer t) -> Value Void	--

-- Use this to ensure type-correctness
mkTemp :: Int -> Type -> Value t
mkTemp n t = Temporary (T n) t :: Value t

valueType :: Value t -> Type
valueType (Temporary _ t)	= t
valueType (Const _)		= i32
valueType (Binop _ e _)		= valueType e
valueType (Load e)		= case (valueType e) of Pointer t -> t
valueType (Store _ _)		= Void

showType :: Value t -> String
showType = show . valueType

instance Show (Value t) where
	show e@(Temporary n _)	= showType e ++ " " ++ show n
	show e@(Const i)	= showType e ++ " " ++ show i
	show e@(Binop b e1 e2)	= showType e ++ " " ++ show b ++ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
	show e@(Load e1)	= showType e ++ " load " ++ show e1
	show e@(Store e1 e2)	= showType e ++ " store " ++ show e1 ++ ", " ++ show e2


-- Instructions
type Label = String
data Instruction where
	Label		:: Label -> Instruction				-- <label>:
	Return		:: Value t -> Instruction			-- ret <type> <value>
	ReturnVoid	:: Instruction					-- ret void
	Branch		:: Value i1 -> Label -> Label -> Instruction	-- br i1 <cond>, label <iftrue>, label <iffalse>
	BranchAlways	:: Label -> Instruction				-- br label <dest>
	Expression	:: Temporary -> Value t -> Instruction		-- %t = <expr>

instance Show Instruction where
	show (Label l)		= l ++ ":"
	show (Return e)		= "ret " ++ show e
	show (ReturnVoid)	= "ret void"
	show (Branch e l1 l2)	= "br " ++ show e ++ ", label " ++ l1 ++ ", label " ++ l2
	show (BranchAlways l)	= "br label " ++ l
	show (Expression t e)	= show t ++ " = " ++ show e

isLabel :: Instruction -> Bool
isLabel (Label _) = True
isLabel _ = False

showIndented :: Instruction -> String
showIndented i = if isLabel i
	then show i
	else "\t" ++ show i


-- Putting it together
type BasicBlock = [Instruction] -- ends always in an terminal (= ret/br)
data Function = Function String [(Type, Temporary)] [BasicBlock] Type -- name args body retType

instance Show Function where
	show (Function name args body retType) = "define " ++ show retType ++ " @" ++ name ++ "(" ++ "){\n" ++ bodyStr ++ "}"
		where bodyStr = unlines $ fmap showIndented (concat body)

type Program = [Function]

showProgram :: Program -> String
showProgram l = unlines $ fmap show l
