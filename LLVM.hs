{-# LANGUAGE GADTs, DeriveDataTypeable #-}

module LLVM where --(Type(..), Binop(..), Temporary(..), Value(Const, Binop, Load, Store), Instruction(..), Function(..), mkTemp, Program, showProgram) where

import Data.Data
import Data.List
import Data.Char
import Utils (implode)


-- TODO: Calls, Builtins, Structs, Globals, Moves, ...


-- Types we have
-- Simplifications: only integral types (no floats), only signed integers
type TypeName = String

data Type where
	Array :: Int -> Type -> Type		-- [<# elements> x <elementtype>]
	FunctionType :: Type -> [Type] -> Type	-- <returntype> (<parameter list>)
	IntegralType :: Int -> Type		-- iN
	Void :: Type				-- void
	Struct :: [Type] -> Type		-- type { <type list> } (non-packed)
	Pointer :: Type -> Type
	NamedType :: TypeName -> Type
	EtceteraType :: Type
	deriving (Eq, Ord)

instance Show Type where
	show (Array n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
	show (FunctionType r l) = show r ++ "(" ++ concat (intersperse "," (map show l)) ++ ")"
	show (IntegralType n) = "i" ++ show n
	show (Void) = "void"
	show (Struct l) = "{" ++ concat (intersperse "," (map show l)) ++ "}"
	show (Pointer t) = show t ++ "*"
	show (NamedType str) = "%" ++ str
	show (EtceteraType) = "..."

i32 :: Type -- The default Integer type
i32 = IntegralType 32

i8 :: Type
i8 = IntegralType 8

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
newtype GlobalName = G String
instance Show GlobalName where
	show (G s) = "@" ++ s

-- Expressions
data Value
	= Temporary Type Temporary	-- %t
	| Global Type GlobalName	-- @bla
	| Const Type Int		-- <n>
	| Null Type			-- null
	| Undef Type			-- undef

valueType :: Value -> Type
valueType (Temporary t _)	= t
valueType (Global t _)		= t
valueType (Const t _)		= t
valueType (Undef t)		= t
valueType (Null t)		= t

showType :: Value -> String
showType = show . valueType

instance Show Value where
	show e@(Temporary _ t)	= show t
	show e@(Global _ g)	= show g
	show e@(Const _ i)	= show i
	show e@(Null _)		= "null"
	show e@(Undef _)	= "undef"

-- Instructions
type Label = String
data Instruction where
	Label		:: Label -> Instruction				-- <label>:
	Decl		:: Temporary -> Instruction -> Instruction	-- %t = <instr>
	Return		:: Value -> Instruction				-- ret <type> <value>
	ReturnVoid	:: Instruction					-- ret void
	Branch		:: Value -> Label -> Label -> Instruction	-- br i1 <cond>, label <iftrue>, label <iffalse>
	BranchAlways	:: Label -> Instruction				-- br label <dest>
	Binop		:: Binop -> Value -> Value -> Instruction	-- add <ty> <op1>, <op2>
	Compare		:: Comparisons -> Value -> Value -> Instruction	-- icmp <cmp> i1 <op1>, <op2>
	Alloca		:: Type -> Instruction				-- alloca <ty>
	Load		:: Value -> Instruction				-- load <ty>* <e>
	Store		:: Value -> Value -> Instruction		-- store <ty> <value>, <ty>* <pointer>
	GetElementPtr	:: Value -> [Value] -> Instruction		-- getelementptr <ty>* <ptr>{, <ty> <idx>}+
	ExtractValue	:: Value -> [Int] -> Instruction		-- extractvalue <at> <val>{, <idx>}+
	InsertValue	:: Value -> Value -> [Int] -> Instruction	-- insertvalue <at> <val>, <ty> <elt>{, <idx>}+
	Call		:: Type -> GlobalName -> [Value] -> Instruction	-- [tail] call <ty> <fnptrval>(<args>)
	PtrToInt	:: Value -> Type -> Instruction			-- ptrtoint <ty> <value> to <ty2>
	Bitcast		:: Value -> Type -> Instruction			-- bitcast <ty> <value> to <ty2>

(+++) x y = x ++ " " ++ y
instance Show Instruction where
	show (Label l)		= l ++ ":"
	show (Decl t i)		= show t +++ "=" +++ show i
	show (Return e)		= "ret" +++ showType e +++ show e
	show (ReturnVoid)	= "ret void"
	show (Branch e l1 l2)	= "br" +++ showType e +++ show e ++ ", label %" ++ l1 ++ ", label %" ++ l2
	show (BranchAlways l)	= "br label %" ++ l
	show (Binop b e1 e2)	= show b +++ showType e1 +++ show e1 ++ ", " ++ show e2
	show (Compare c e1 e2)	= "icmp" +++ show c +++ showType e1 +++ show e1 ++ ", " ++ show e2
	show (Alloca t)		= "alloca" +++ show t
	show (Load e)		= "load" +++ showType e +++ show e
	show (Store e1 e2)	= "store" +++ showType e1 +++ show e1 ++ ", " ++ showType e2 +++ show e2
	show (GetElementPtr v idxs) = "getelementptr" +++ showType v +++ show v ++ concat (map showidx idxs)
		where showidx x = "," +++ showType x +++ show x
	show (ExtractValue v idxs) = "extractvalue" +++ showType v +++ show v ++ concat (map showidx idxs)
		where showidx x = "," +++ show x
	show (InsertValue v elt idxs) = "insertvalue" +++ showType v +++ show v ++ ", " ++ showType elt +++ show elt ++ concat (map showidx idxs)
		where showidx x = "," +++ show x
	show (Call t f args)	= "call" +++ show t +++ show f +++ "(" ++ concat (intersperse ", " $ map showarg args) ++ ")"
		where showarg x = showType x +++ show x
	show (PtrToInt v t)	= "ptrtoint" +++ showType v +++ show v +++ "to" +++ show t
	show (Bitcast v t)	= "bitcast" +++ showType v +++ show v +++ "to" +++ show t

isLabel :: Instruction -> Bool
isLabel (Label _) = True
isLabel _ = False

showIndented :: Instruction -> String
showIndented i = if isLabel i
	then show i
	else "\t" ++ show i


-- Putting it together
type BasicBlock = [Instruction] -- ends always in an terminal (= ret/br)
data Function = Function GlobalName [(Type, Temporary)] [BasicBlock] Type -- name args body retType

instance Show Function where
	show (Function name args body retType) = "define " ++ show retType +++ show name ++ "(" ++ argsStr ++ ") {\n" ++ bodyStr ++ "}"
		where
			argsStr = implode ", " $ flip map args $ \(t, temp) -> show t +++ show temp
			bodyStr = unlines $ fmap showIndented (concat body)

data Program = Prog [(GlobalName, Type)] [(TypeName, Type)] [Function]

defaultHeaders = [
	"declare i8* @malloc(i32)",
	"declare i32 @printf(i8*, ...) nounwind",
	"@printf_arg = internal constant [4 x i8] c\"%d\\0A\\00\""
	]

showGlob :: (GlobalName, Type) -> String
showGlob (name, t) = show name +++ "=" +++ "global" +++ show t +++ "undef"

showTypes :: (TypeName, Type) -> String
showTypes (name, t) = "%" ++ name +++ "=" +++ "type" +++ show t

showProgram :: Program -> String
showProgram (Prog globs types fs) = unlines $ defaultHeaders ++ [""] ++ (map showGlob globs) ++ (map showTypes types) ++ (fmap show fs)
