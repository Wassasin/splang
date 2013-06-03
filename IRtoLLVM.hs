{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoLLVM where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))
import Utils

import qualified AST
import IR
import qualified LLVM

data TranslationState = TranslationState
	{ llvmTemporary :: Int }

emptyState = TranslationState { llvmTemporary = 0 }

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
		fs <- mapM translate fs
		return fs

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
	translate (Move (Data ty n) e2) = error "COMPILER BUG: Move to Data not yet implemented"
	translate (Move (Temp ty n) e2) = do
		(s1, Just (LLVM.Temporary _ t1)) <- translate (Temp ty n)
		(s2, Just t2) <- translate e2
		return $ s1 ++ s2 ++ [LLVM.Decl t1 (LLVM.Return t2)]
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
	translate (Temp ty n)		= do
		ty <- translate ty
		onlyValue $ LLVM.Temporary ty (LLVM.T n)
	translate (Data ty n)		= error "COMPILER BUG: No data yet"
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
