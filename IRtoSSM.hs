{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module IRtoSSM where

import Control.Monad.State
import Control.Monad.Writer

-- We currently need this, because IR binops are AST binops
import qualified AST
import IR
import qualified SSM

type Output = [SSM.Instruction]
type TranslationState = ()

-- Emit a single instruction
out :: SSM.Instruction -> WriterT Output (State TranslationState) ()
out x = tell [x]

-- Add information about the stack and where the functions/labels are
emptyState :: TranslationState
emptyState = ()

getFunctionLocation :: Label -> State TranslationState Int
getFunctionLocation _ = return 0

getTemporaryLocation :: Temporary -> State TranslationState Int
getTemporaryLocation _ = return 0

-- We use the Writer monad to automatically apply ++ everywhere, and the State monad for information about stack/functions/labels
class Translate a where
	translate :: a -> WriterT Output (State TranslationState) ()

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
		location <- lift $ getFunctionLocation label
		out (SSM.LoadConstant location)
		out (SSM.JumpToSubroutine)


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

translateIt :: IRExpr -> Output
translateIt expr = flip evalState emptyState . execWriterT $ translate expr
