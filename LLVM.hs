{-# LANGUAGE GADTs, DeriveDataTypeable #-}

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
data Binop = Add | Sub | Mul | SDiv | SRem | And | Or
	deriving (Eq, Typeable, Data)

instance Show Binop where
	show = map toLower . showConstr . toConstr

-- Sorry for being lazy here... Only signed types
data Comparisons = Eq | Ne | Sgt | Sge | Slt | Sle
	deriving (Eq, Typeable, Data)

instance Show Comparisons where
	show = map toLower . showConstr . toConstr

-- Variables
newtype Temporary = T Int
instance Show Temporary where
	show (T n) = "%" ++ show n


-- Expressions
data Value
	= Temporary Type Temporary	-- %t
	| Const Type Int		-- <n>
	| Binop Binop Value Value 	-- <binop> <ty> <op1>, <op2>
	| Compare Comparisons Value Value -- icmp <cond> <ty> <op1>, <op2>
	| Load Value			--

valueType :: Value -> Type
valueType (Temporary t _)	= t
valueType (Const t _)		= t
valueType (Binop _ e _)		= valueType e
valueType (Compare _ _ _)	= i1
valueType (Load e)		= case (valueType e) of Pointer t -> t

showType :: Value -> String
showType = show . valueType

instance Show Value where
	show e@(Temporary _ n)	= showType e ++ " " ++ show n
	show e@(Const _ i)	= showType e ++ " " ++ show i
	show e@(Binop b e1 e2)	= showType e ++ " " ++ show b ++ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
	show e@(Compare c e1 e2)= showType e ++ " icmp " ++ show c ++ "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
	show e@(Load e1)	= showType e ++ " load " ++ show e1


-- Instructions
type Label = String
data Instruction where
	Label		:: Label -> Instruction				-- <label>:
	Return		:: Value -> Instruction				-- ret <type> <value>
	ReturnVoid	:: Instruction					-- ret void
	Branch		:: Value -> Label -> Label -> Instruction	-- br i1 <cond>, label <iftrue>, label <iffalse>
	BranchAlways	:: Label -> Instruction				-- br label <dest>
	Expression	:: Temporary -> Value -> Instruction		-- %t = <expr>
	Store		:: Value -> Value -> Instruction		-- store <ty> <value>, <ty>* <pointer>

instance Show Instruction where
	show (Label l)		= l ++ ":"
	show (Return e)		= "ret " ++ show e
	show (ReturnVoid)	= "ret void"
	show (Branch e l1 l2)	= "br " ++ show e ++ ", label " ++ l1 ++ ", label " ++ l2
	show (BranchAlways l)	= "br label " ++ l
	show (Expression t e)	= show t ++ " = " ++ show e
	show (Store e1 e2)	= "store " ++ show e1 ++ ", " ++ show e2

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
