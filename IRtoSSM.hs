{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, TupleSections #-}

module IRtoSSM where

import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldl, map)


-- We currently need this, because IR binops are AST binops
import qualified AST
import IR
import qualified SSM

-- Obvious output
type Output = SSM.Program

-- Can be absolute or relative addresses
type Address = Int

data TemporaryKind
	= Global
	| Argument
	| Local

-- Information about the stack and temporaries
-- currentFunction is the enclosing function (handy for getting args, if needed)
-- temporaryLocations gives information about the temps
-- stackPtr is the stack pointer relative to the MP
data TranslationState = TranslationState
	{ currentFunction :: Maybe (IRFunc [BasicBlock])
	, temporaryLocations :: Map Temporary (Address, TemporaryKind)
	, stackPtr :: Address }

emptyState :: TranslationState
emptyState = TranslationState
	{ currentFunction = Nothing
	, temporaryLocations = empty
	, stackPtr = 0 }

increaseStackPtr :: TranslationState -> TranslationState
increaseStackPtr o = o { stackPtr = 1 + (stackPtr o) }

decreaseStackPtr :: TranslationState -> TranslationState
decreaseStackPtr o = o { stackPtr = (stackPtr o) - 1 }

assignCurrentFunction :: IRFunc [BasicBlock] -> TranslationState -> TranslationState
assignCurrentFunction f@(Func _ args _ _) o = o
	{ currentFunction = Just f
	, temporaryLocations = inserts $ temporaryLocations o
	, stackPtr = 0 }
	where
		-- TODO: figure out real size of an argument
		locs = map (,Argument) [(-1), (-2) ..]
		ts = zip (map snd args) locs
		fs = map (uncurry insert) ts
		inserts = foldl (.) id fs

-- Emit a single instruction
out :: SSM.Instruction -> WriterT Output (State TranslationState) ()
out x = tell [x]

-- Returns relative position to MP for arguments,
-- absolute position form 0 for globals
-- If it couldn't be found, we asume it is a new temporary, and add it
getTemporaryLocation :: Temporary -> State TranslationState (Maybe (Address, TemporaryKind))
getTemporaryLocation t = Map.lookup t <$> temporaryLocations <$> get

loadTemporary :: (Address, TemporaryKind) -> WriterT Output (State TranslationState) ()
loadTemporary (location, Global)	= modify increaseStackPtr >> out (SSM.LoadViaAddress location)
loadTemporary (location, Argument)	= modify increaseStackPtr >> out (SSM.LoadLocal location)
loadTemporary (location, Local)		= modify increaseStackPtr >> out (SSM.LoadLocal location)


-- We use the Writer monad to automatically apply ++ everywhere, and the State monad for information about stack/temporaries
class Translate a where
	translate :: a -> WriterT Output (State TranslationState) ()

instance Translate [IRFunc [BasicBlock]] where
	translate fs = mapM_ translate fs

-- TODO: arguments/returning/etc
instance Translate (IRFunc [BasicBlock]) where
	translate f@(Func l _ body _) = do
		lift . modify $ assignCurrentFunction f
		out (SSM.Label l)
		out (SSM.NoOperation)
		translate body

instance Translate [BasicBlock] where
	translate bbs = mapM_ translate bbs

instance Translate BasicBlock where
	translate bb = mapM_ translate bb

instance Translate IRStmt where
	-- TODO: Move
	translate (Move (Temp _ t) e2) = do
		thing <- lift $ getTemporaryLocation t
		case thing of
			Nothing -> do
				translate e2 -- It simply lives on the stack
			Just (y, Global) -> do
				translate e2
				out (SSM.StoreViaAddress y)
				lift $ modify decreaseStackPtr
			Just (y, _) -> do
				translate e2
				out (SSM.StoreLocal y)
				lift $ modify decreaseStackPtr
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
	-- TODO: Figure out whether this is sane semantics for Temps
	translate (Temp _ t) = do
		t <- lift $ getTemporaryLocation t
		case t of
			Just n -> loadTemporary n
			Nothing -> return () -- It lives on the stack?
	translate (Binop e1 bop e2) = do
		translate e1
		translate e2
		translate bop
		lift $ modify decreaseStackPtr
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

-- TODO: compare operators
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
