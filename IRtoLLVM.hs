{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoLLVM where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))

import qualified AST
import IR
import qualified LLVM

type TranslationState = ()

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
		return $ LLVM.Function l args bbs retType -- name args body retType

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
	translate stmts = mapM translate stmts

instance Translate IRStmt LLVM.Instruction where
	translate (Move e1 e2)		= error "COMPILER BUG: Move not yet implemented"
	translate (Expression e)	= do
		e <- translate e
		return $ LLVM.Expression (LLVM.T 0) e
	translate (Jump l)		= return $ LLVM.BranchAlways l
	translate (CJump e l1 l2)	= do
		e <- translate e
		return $ LLVM.Branch e l1 l2
	translate (Ret (Just e))	= do
		e <- translate e
		return $ LLVM.Return e
	translate (Ret Nothing)		= return LLVM.ReturnVoid
	translate (Label l)		= return $ LLVM.Label l
	translate Nop			= error "COMPILER BUG: Nop doesnt exist yet"

isComparison :: AST.BinaryOperator a -> Bool
isComparison (AST.Equals _)		= True
isComparison (AST.LesserThan _)		= True
isComparison (AST.GreaterThan _)	= True
isComparison (AST.LesserEqualThan _)	= True
isComparison (AST.GreaterEqualThan _)	= True
isComparison (AST.Nequals _)		= True
isComparison _				= False

instance Translate IRExpr LLVM.Value where
	translate (Const Bool (-1))	= return $ LLVM.Const LLVM.i1 1
	translate (Const Bool _)	= return $ LLVM.Const LLVM.i1 0
	translate (Const Int n)		= return $ LLVM.Const LLVM.i32 n
	translate (Temp ty n)		= do
		ty <- translate ty
		return $ LLVM.Temporary ty (LLVM.T n)
	translate (Data ty n)		= error "COMPILER BUG: No data yet"
	translate (Binop ty e1 (AST.Cons _) e2) = error "COMPILER BUG: No cons yet"
	translate (Binop ty e1 b e2)	= do
		e1 <- translate e1
		e2 <- translate e2
		if isComparison b
			then return $ LLVM.Compare (translateComparison b) e1 e2
			else return $ LLVM.Binop (translateBinop b) e1 e2
	translate (Unop ty uop e)	= do
		-- Note: Only unops are Not and Negate, both are "0 - x"
		e <- translate e
		let t = LLVM.valueType e -- Or translate ty?
		return $ LLVM.Binop LLVM.Sub (LLVM.Const t 0) e
	translate (Call mt l es)	= error "COMPILER BUG: No call yet"
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
irToLLVM = flip evalState () . translate
