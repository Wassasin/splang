{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoLLVM where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldr, foldl, map)
import Utils

import qualified AST
import IR
import qualified LLVM

data TranslationState = TranslationState
	{ llvmTemporary :: Int
	, dataPointers :: Map Temporary LLVM.Value }

emptyState = TranslationState { llvmTemporary = 0, dataPointers = empty }

getDataPointer :: Temporary -> State TranslationState (Maybe LLVM.Value)
getDataPointer t = Map.lookup t <$> dataPointers <$> get

saveDataPointer :: Temporary -> LLVM.Value -> TranslationState -> TranslationState
saveDataPointer t v o = o { dataPointers = insert t v $ dataPointers o }

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
		return $ LLVM.Prog gd [] (main:fs) -- TODO: type decls

instance Translate (IRFunc [BasicBlock]) LLVM.Function where
	translate (Func l args bbs retType) = do
		args <- mapM (\(x, y) -> do
			x <- translate x
			y <- return $ LLVM.T y
			return (x, y)) args
		bbs <- mapM translate bbs
		retType <- translate retType
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
		t <- translate t
		-- This is probably incorrect
		let node = LLVM.Struct [LLVM.Pointer node, t]
		return $ LLVM.Pointer node
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
	translate (Unop ty uop e)	= do
		-- Note: Only unops are Not and Negate, both are "0 - x"
		ty <- translate ty
		(s, Just e) <- translate e
		temp <- generateTemporary
		let temp2 = LLVM.Temporary ty temp
		return2 $$ s ++ [LLVM.Decl temp $ LLVM.Binop LLVM.Sub (LLVM.Const ty 0) e] $$ Just temp2
	translate (Call mt l es)	= do
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
	translate (Builtin mt b)	= error "COMPILER BUG: No builtin yet"
	
translateIRExprTemp :: Type -> Temporary -> State TranslationState ([LLVM.Instruction], Maybe LLVM.Value)
translateIRExprTemp ty n = do
	ty <- translate ty
	Just ptr <- getDataPointer n
	temp <- generateTemporary
	let tempe = LLVM.Temporary ty temp
	return2 $$ [LLVM.Decl temp $ LLVM.Load ptr] $$ Just tempe

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

irToLLVM :: Program [BasicBlock] -> LLVM.Program
irToLLVM = flip evalState emptyState . translate
