{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE GADTs, DataKinds, EmptyDataDecls, KindSignatures, ScopedTypeVariables, DeriveDataTypeable #-}

module IRtoLLVM where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))

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
	translate _	= error "COMPILER BUG: No other types for LLVM" -- Pair t1 t2, ListPtr t, ListAbstractEmpty

instance Translate BasicBlock LLVM.BasicBlock where
	translate stmts = mapM translate stmts

instance Translate IRStmt LLVM.Instruction where
	translate (Move e1 e2)		= error "COMPILER BUG: Move not yet implemented"
	translate (Expression e)	= do
		e <- translateValue e
		return $ LLVM.Expression (LLVM.T 0) e
	translate (Jump l)		= return $ LLVM.BranchAlways l
	translate (CJump e l1 l2)	= do
		e <- translateValue e
		return $ LLVM.Branch e l1 l2
	translate (Ret (Just e))	= do
		e <- translateValue e
		return $ LLVM.Return e
	translate (Ret Nothing)		= return LLVM.ReturnVoid
	translate (Label l)		= return $ LLVM.Label l
	translate Nop			= error "COMPILER BUG: Nop doesnt exist yet"

-- I couldn't figure out how to type this... Inference luckily works :)
translateValue (Const Bool b)	= error "COMPILER BUG: No bool constants yet"
translateValue (Const Int n)		= return $ LLVM.Const n
translateValue (Temp ty n)		= do
	ty <- translate ty
	return $ LLVM.mkTemp n ty
translateValue (Data ty n)		= error "COMPILER BUG: No data yet"
translateValue (Binop ty e1 b e2)	= do
	e1 <- translateValue e1
	e2 <- translateValue e2
	return $ LLVM.Binop LLVM.Add e1 e2
translateValue (Unop ty uop e)	= error "COMPILER BUG: No unop yet"
translateValue (Call mt l es)	= error "COMPILER BUG: No call yet"
translateValue (Builtin mt b)	= error "COMPILER BUG: No builtin yet"

irToLLVM :: Program [BasicBlock] -> LLVM.Program
irToLLVM = flip evalState () . translate
