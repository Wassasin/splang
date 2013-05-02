{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoSSM where

import Control.Monad.State
import Control.Monad.Writer

-- We currently need this, because IR binops are AST binops
import qualified AST
import IR
import qualified SSM

-- Obvious output
type Output = SSM.Program

-- Add information about the stack and temporaries
type TranslationState = ()
emptyState :: TranslationState
emptyState = ()

-- Emit a single instruction
out :: SSM.Instruction -> WriterT Output (State TranslationState) ()
out x = tell [x]

getTemporaryLocation :: Temporary -> State TranslationState Int
getTemporaryLocation _ = return 0


-- We use the Writer monad to automatically apply ++ everywhere, and the State monad for information about stack/temporaries
class Translate a where
	translate :: a -> WriterT Output (State TranslationState) ()

instance (Translate a) => Translate [IRFunc a] where
	translate fs = mapM_ translate fs

-- TODO: arguments/returning/etc
instance (Translate a) => Translate (IRFunc a) where
	translate (Func l _ body _) = do
		out (SSM.Label l)
		translate body

instance Translate [BasicBlock] where
	translate bbs = mapM_ translate bbs

instance Translate BasicBlock where
	translate bb = mapM_ translate bb

instance Translate IRStmt where
	-- TODO: Move
	translate (Move e1 e2) = do
		translate e1
		translate e2
		out (SSM.StoreLocal 0)
	translate (Expression e) = translate e
	translate (Jump label) = out (SSM.BranchAlways label)
	translate (CJump e tl fl) = do
		translate e
		out (SSM.BranchOnTrue tl)
		out (SSM.BranchOnFalse fl)
	translate (Seq s1 s2) = do
		translate s1
		translate s2
	translate (Ret (Just e)) = do
		translate e
		out (SSM.Return)
	-- TODO: probably more than just return
	translate (Ret Nothing) = out (SSM.Return)
	translate (Label l) = out (SSM.Label l)
	translate (Nop) = out (SSM.NoOperation)

instance Translate IRExpr where
	translate (Const _ n) = do
		out (SSM.LoadConstant n)
	translate (Temp _ t) = do
		location <- lift $ getTemporaryLocation t
		out (SSM.LoadLocal location)
	translate (Binop e1 bop e2) = do
		translate e1
		translate e2
		translate bop
	translate (Unop uop e) = do
		translate e
		translate uop
	translate (Mem e) = do
		translate e
		out (SSM.LoadViaAddress 0)
	translate (Call label args) = do
		mapM translate args
		out (SSM.BranchToSubroutine label)
	-- Should never occur
	translate (Eseq _ _) = error "COMPILER BUG: Eseq present in IR"

instance Translate IRBOps where
	translate (AST.Multiplication _)	= out SSM.Multiply
	translate (AST.Division _)		= out SSM.Divide
	translate (AST.Modulo _)		= out SSM.Modulo
	translate (AST.Plus _)			= out SSM.Add
	translate (AST.Minus _)			= out SSM.Substraction
	translate (AST.And _)			= out SSM.And
	translate (AST.Or _)			= out SSM.Or

instance Translate IRUOps where
	translate (AST.Not _)			= out SSM.Not
	translate (AST.Negative _)		= out SSM.Negation


irToSSM :: Translate a => a -> SSM.Program
irToSSM = flip evalState emptyState . execWriterT . translate

-- For testing/debugging
printSSM :: Translate a => a -> IO ()
printSSM = putStrLn . SSM.showProgram . irToSSM

f :: IRStmt -> IO ()
f = printSSM . linearize
