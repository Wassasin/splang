{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoLLVM where

import Control.Monad.State
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldr, foldl, map)
import Data.Tuple (swap)
import Data.List (null)
import Utils

import qualified AST
import IR
import qualified LLVM

data TranslationState = TranslationState
	{ llvmTemporary :: Int
	, dataPointers :: Map Temporary LLVM.Value
	, namedTypes :: Map LLVM.Type LLVM.TypeName }

emptyState = TranslationState { llvmTemporary = 0, dataPointers = Map.empty, namedTypes = Map.empty }

getDataPointer :: Temporary -> State TranslationState (Maybe LLVM.Value)
getDataPointer t = Map.lookup t <$> dataPointers <$> get

saveDataPointer :: Temporary -> LLVM.Value -> TranslationState -> TranslationState
saveDataPointer t v o = o { dataPointers = Map.insert t v $ dataPointers o }

setUsingTypeName :: LLVM.Type -> LLVM.TypeName -> TranslationState -> TranslationState
setUsingTypeName t str o = o { namedTypes = Map.insert t str $ namedTypes o }

generateTemporary :: State TranslationState LLVM.Temporary
generateTemporary = do
	state <- get
	let temp = LLVM.T (llvmTemporary state)
	let newState = state { llvmTemporary = (llvmTemporary state + 1) }
	put newState
	return temp

class Translate a b | a -> b where
	translate :: a -> State TranslationState b

instance Translate (Program [BasicBlock]) LLVM.Program where
	translate (fs, gs) = do
		gd <- flip mapM gs $ \(Glob i gt _) -> do
			gt <- translate gt
			let name = LLVM.G $ "glob" ++ show i
			modify $ saveDataPointer i $ LLVM.Global (LLVM.Pointer gt) name
			return (name, gt)
		let gc = flip map gs $ \(Glob _ _ l) -> LLVM.Call LLVM.Void (LLVM.G l) []
		let mainc = LLVM.Call LLVM.Void (LLVM.G "main_v") []
		fs <- flip mapM fs $ \f -> do
			modify $ \s -> s { llvmTemporary = 0 } -- LLVM wants reset temporaries at beginning of FunDecl
			translate f
		let main = LLVM.Function (LLVM.G "main") [] [gc ++ [mainc] ++ [LLVM.ReturnVoid]] LLVM.Void
		s <- get
		let td = map swap $ Map.toList $ namedTypes s
		return $ LLVM.Prog gd td (main:fs)

instance Translate (IRFunc [BasicBlock]) LLVM.Function where
	translate (Func l args bbs retType) = do
		argstup <- mapM (\(t, n) -> do
			t <- translate t
			let name = LLVM.N $ "arg" ++ show n
			let arg = LLVM.NamedTemporary t name
			temp <- generateTemporary
			let tempe = LLVM.Temporary (LLVM.Pointer t) temp
			modify $ saveDataPointer n tempe
			let alloca = LLVM.Decl temp $ LLVM.Alloca t
			let store = LLVM.Store arg tempe
			return ((t, name), [alloca, store])) args
		let (args, argstmts) = unzip argstup
		bbs <- mapM translate bbs
		retType <- translate retType
		bbs <- return $ if Data.List.null args then bbs else [LLVM.Label "argcopy" : (concat argstmts) ++ [LLVM.BranchAlways $ LLVM.fetchLabel $ head bbs]] ++ bbs
		return $ LLVM.Function (LLVM.G l) args bbs retType -- name args body retType	

instance Translate (Maybe Type) LLVM.Type where
	translate Nothing = return $ LLVM.Void
	translate (Just t) = translate t

instance Translate Type LLVM.Type where
	translate Bool	= return $ LLVM.i1
	translate Int	= return $ LLVM.i32
	translate (Pair t1 t2) = do
		t1 <- translate t1
		t2 <- translate t2
		return $ LLVM.Struct [t1, t2]
	translate (ListPtr t) = do
		let str = "cons_" ++ mangle t
		let namedType = LLVM.NamedType str
		t <- translate t
		let node = LLVM.Struct [LLVM.Pointer namedType, t]
		modify $ setUsingTypeName node str
		return $ LLVM.Pointer namedType
	translate ListAbstractEmpty = return $ LLVM.Pointer LLVM.Void

instance Translate BasicBlock LLVM.BasicBlock where
	translate stmts = concat <$> mapM translate stmts

instance Translate IRStmt [LLVM.Instruction] where
	translate (Move (Data ty n) e) = translateIRStmtMove ty n e
	translate (Move (Temp ty n) e) = translateIRStmtMove ty n e
	translate (Expression e)	= do
		(s, e) <- translate e
		return $ s
	translate (Jump l)		= return [LLVM.BranchAlways l]
	translate (CJump e l1 l2)	= do
		(s, Just e) <- translate e
		return $ s ++ [LLVM.Branch e l1 l2]
	translate (Ret (Just e))	= do
		(s, Just e) <- translate e
		return $ s ++ [LLVM.Return e]
	translate (Ret Nothing)		= return [LLVM.ReturnVoid]
	translate (Label l)		= return [LLVM.Label l]
	translate Nop			= return []

translateIRStmtMove :: Type -> Temporary -> IRExpr -> State TranslationState [LLVM.Instruction]
translateIRStmtMove ty n e = do
		ty <- translate ty
		(s, Just e) <- translate e
		thing <- getDataPointer n
		case thing of
			-- New local variable
			Nothing -> do
				ptr <- generateTemporary
				let ptre = LLVM.Temporary (LLVM.Pointer ty) ptr
				modify $ saveDataPointer n ptre
				return $ s ++ [
					LLVM.Decl ptr $ LLVM.Alloca ty,
					LLVM.Store e ptre]
			-- Assignment
			Just ptr -> do
				return $ s ++ [LLVM.Store e ptr]

isComparison :: AST.BinaryOperator a -> Bool
isComparison (AST.Equals _)		= True
isComparison (AST.LesserThan _)		= True
isComparison (AST.GreaterThan _)	= True
isComparison (AST.LesserEqualThan _)	= True
isComparison (AST.GreaterEqualThan _)	= True
isComparison (AST.Nequals _)		= True
isComparison _				= False

onlyValue v = return ([], Just v)
return2 x y = return (x, y)
infixl 0 $$
($$) = ($) -- handy for multiple arguments
instance Translate IRExpr ([LLVM.Instruction], Maybe LLVM.Value) where
	translate (Const Bool (-1))	= onlyValue $ LLVM.Const LLVM.i1 1
	translate (Const Bool _)	= onlyValue $ LLVM.Const LLVM.i1 0
	translate (Const Int n)		= onlyValue $ LLVM.Const LLVM.i32 n
	translate (Const t@(ListPtr _) 0)	= do
		t <- translate t
		onlyValue $ LLVM.Null t
	translate (Temp ty n)		= translateIRExprTemp ty n
	translate (Data ty n)		= translateIRExprTemp ty n
	translate (Binop ty e1 (AST.Cons _) e2) = error "COMPILER BUG: No cons yet"
	translate (Binop ty e1 b e2)	= do
		ty <- translate ty
		(s1, Just e1) <- translate e1
		(s2, Just e2) <- translate e2
		temp <- generateTemporary
		let temp2 = LLVM.Temporary ty temp
		if isComparison b
			then return2 $$ s1 ++ s2 ++ [LLVM.Decl temp $ LLVM.Compare (translateComparison b) e1 e2] $$ Just temp2
			else return2 $$ s1 ++ s2 ++ [LLVM.Decl temp $ LLVM.Binop (translateBinop b) e1 e2] $$ Just temp2
	translate (Unop ty (AST.Negative ()) e)	= do
		-- Note: Only unops are Not and Negate, both are "0 - x"
		ty <- translate ty
		(s, Just e) <- translate e
		temp <- generateTemporary
		let temp2 = LLVM.Temporary ty temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.Binop LLVM.Sub (LLVM.Const ty 0) e] $$ Just temp2
	translate (Unop ty (AST.Not ()) e)	= do
		-- Note: Only unops are Not and Negate, both are "0 - x"
		ty <- translate ty
		(s, Just e) <- translate e
		temp <- generateTemporary
		let temp2 = LLVM.Temporary ty temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.Binop LLVM.Sub (LLVM.Const ty 1) e] $$ Just temp2
	translate (Call (Just mt) l es)	= do
		ty <- translate mt
		args <- mapM translate es
		temp <- generateTemporary
		let tempe = LLVM.Temporary ty temp
		return2 $$ concat (map fst args) ++ [LLVM.Decl temp $ LLVM.Call ty (LLVM.G l) (map (guardJust "" . snd) args)] $$ Just tempe
	translate (Call mt@Nothing l es)	= do
		ty <- translate mt
		args <- mapM translate es
		return2 $$ concat (map fst args) ++ [LLVM.Call ty (LLVM.G l) (map (guardJust "" . snd) args)] $$ Nothing
	translate (Builtin t (MakePair e1 e2)) = do
		t <- translate t
		(s1, Just e1) <- translate e1
		(s2, Just e2) <- translate e2
		temp1 <- generateTemporary
		let temp1e = LLVM.Temporary t temp1
		temp2 <- generateTemporary
		let temp2e = LLVM.Temporary t temp2
		return2 $$ s1 ++ s2 ++ [
				LLVM.Decl temp1 $ LLVM.InsertValue (LLVM.Undef t) e1 [0],
				LLVM.Decl temp2 $ LLVM.InsertValue temp1e e2 [1]
			] $$ Just temp2e
	translate (Builtin t (IR.First e)) = do
		t <- translate t
		(s, Just e) <- translate e
		temp <- generateTemporary
		let tempe = LLVM.Temporary t temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.ExtractValue e [0]] $$ Just tempe
	translate (Builtin t (IR.Second e)) = do
		t <- translate t
		(s, Just e) <- translate e
		temp <- generateTemporary
		let tempe = LLVM.Temporary t temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.ExtractValue e [1]] $$ Just tempe
	translate (Builtin t (IR.Cons x xs)) = do
		consptrt@(LLVM.Pointer const) <- translate t
		(s1, Just x) <- translate x
		(s2, Just xs) <- translate xs
		(s3, ptr) <- translateMalloc const
		temp1 <- generateTemporary
		let temp1e = LLVM.Temporary const temp1
		temp2 <- generateTemporary
		let temp2e = LLVM.Temporary const temp2
		return2 $$ s1 ++ s2 ++ s3 ++ [
				LLVM.Decl temp1 $ LLVM.InsertValue (LLVM.Undef const) xs [0],
				LLVM.Decl temp2 $ LLVM.InsertValue temp1e x [1],
				LLVM.Store temp2e ptr
			] $$ Just ptr
	translate (Builtin t (IR.IsEmpty e)) = do
		t <- translate t
		(s, Just e) <- translate e
		temp <- generateTemporary
		let tempe = LLVM.Temporary LLVM.i1 temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.Compare LLVM.Eq e (LLVM.Null $ LLVM.valueType e)] $$ Just tempe
	translate (Builtin t (IR.Tail e)) = do
		consptrt@(LLVM.Pointer const) <- translate t
		(s, Just e) <- translate e
		temp1 <- generateTemporary
		let temp1e = LLVM.Temporary const temp1
		temp2 <- generateTemporary
		let temp2e = LLVM.Temporary consptrt temp2
		return2 $$ s ++ [
				LLVM.Decl temp1 $ LLVM.Load e,
				LLVM.Decl temp2 $ LLVM.ExtractValue temp1e [0]
			] $$ Just temp2e
	translate (Builtin (Just t) (IR.Head e)) = do
		objt <- translate t
		consptrt@(LLVM.Pointer const) <- translate $ ListPtr t
		(s, Just e) <- translate e
		temp1 <- generateTemporary
		let temp1e = LLVM.Temporary const temp1
		temp2 <- generateTemporary
		let temp2e = LLVM.Temporary objt temp2
		return2 $$ s ++ [
				LLVM.Decl temp1 $ LLVM.Load e,
				LLVM.Decl temp2 $ LLVM.ExtractValue temp1e [1]
			] $$ Just temp2e
	translate (Builtin t (Print e)) = do
		t <- translate t
		(s, Just e) <- translate e
		temp <- generateTemporary
		let tempe = LLVM.Temporary (LLVM.Pointer $ LLVM.i8) temp
		generateTemporary -- LLVM wants that we waste a temporary; for the call to Print
		return2 $$ s ++ [
				LLVM.Decl temp $ LLVM.GetElementPtr (LLVM.Global (LLVM.Pointer $ LLVM.Array 4 $ LLVM.i8) $ LLVM.G "printf_arg") [LLVM.Const LLVM.i32 0, LLVM.Const LLVM.i32 0],
				LLVM.Call (LLVM.Pointer $ LLVM.FunctionType LLVM.i32 [LLVM.Pointer LLVM.i8, LLVM.EtceteraType]) (LLVM.G "printf") [tempe, e]
			] $$ Nothing
	translate (Builtin mt b)	= error "COMPILER BUG: No builtin yet"

translateIRExprTemp :: Type -> Temporary -> State TranslationState ([LLVM.Instruction], Maybe LLVM.Value)
translateIRExprTemp ty n = do
	ty <- translate ty
	Just ptr <- getDataPointer n
	temp <- generateTemporary
	let tempe = LLVM.Temporary ty temp
	return2 $$ [LLVM.Decl temp $ LLVM.Load ptr] $$ Just tempe

translateSizeOf :: LLVM.Type -> State TranslationState ([LLVM.Instruction], LLVM.Value)
translateSizeOf t = do
	temp1 <- generateTemporary
	let temp1e = LLVM.Temporary (LLVM.Pointer t) temp1
	temp2 <- generateTemporary
	let temp2e = LLVM.Temporary LLVM.i32 temp2
	return2 $$ [
			LLVM.Decl temp1 $ LLVM.GetElementPtr (LLVM.Null $ LLVM.Pointer t) [LLVM.Const LLVM.i32 1],
			LLVM.Decl temp2 $ LLVM.PtrToInt temp1e LLVM.i32
		] $$ temp2e

translateMalloc :: LLVM.Type -> State TranslationState ([LLVM.Instruction], LLVM.Value)
translateMalloc t = do
	let ptrt = LLVM.Pointer t
	(s, sizeof) <- translateSizeOf t -- size of object we want to malloc
	temp1 <- generateTemporary
	let temp1e = LLVM.Temporary (LLVM.Pointer LLVM.i8) temp1
	temp2 <- generateTemporary
	let temp2e = LLVM.Temporary ptrt temp2
	let malloc = LLVM.Decl temp1 $ LLVM.Call (LLVM.Pointer $ LLVM.FunctionType (LLVM.Pointer LLVM.i8) [LLVM.i32]) (LLVM.G "malloc") [sizeof]
	let cast = LLVM.Decl temp2 $ LLVM.Bitcast temp1e ptrt
	return2 $$ s ++ [malloc, cast] $$ temp2e

translateBinop (AST.Multiplication _)	= LLVM.Mul
translateBinop (AST.Division _)		= LLVM.SDiv
translateBinop (AST.Modulo _)		= LLVM.SRem
translateBinop (AST.Plus _)		= LLVM.Add
translateBinop (AST.Minus _)		= LLVM.Sub
translateBinop (AST.And _)		= LLVM.And
translateBinop (AST.Or _)		= LLVM.Or
translateBinop _			= error "COMPILER BUG: Trying to convert a non-binop as binop"

translateComparison (AST.Equals _)	= LLVM.Eq
translateComparison (AST.LesserThan _)	= LLVM.Slt
translateComparison (AST.GreaterThan _)	= LLVM.Sgt
translateComparison (AST.LesserEqualThan _) = LLVM.Sle
translateComparison (AST.GreaterEqualThan _) = LLVM.Sge
translateComparison (AST.Nequals _)	= LLVM.Ne
translateComparison _			= error "COMPILER BUG: Trying to convert a non-comparison as comparison"

mangle :: Type -> String
mangle Bool			= "b"
mangle Int			= "i"
mangle (Pair x y)		= "p" ++ mangle x ++ mangle y
mangle (ListPtr x)		= "l" ++ mangle x
mangle ListAbstractEmpty	= "e"

irToLLVM :: Program [BasicBlock] -> LLVM.Program
irToLLVM = flip evalState emptyState . translate
