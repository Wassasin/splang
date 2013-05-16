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

data DataKind
	= Global
	| Argument
	| Local


-- Information about the stack and temporaries
-- temporaryLocations gives information about the temps
-- stackPtr is the stack pointer relative to the MP
data TranslationState = TranslationState
	{ temporaryLocations :: Map Temporary (Address, DataKind)
	, stackPtr :: Address }

emptyState = TranslationState
	{ temporaryLocations = empty
	, stackPtr = 0 }

-- Some useful thingies
increaseStackPtr o = o { stackPtr = 1 + (stackPtr o) }
decreaseStackPtr o = o { stackPtr = (stackPtr o) - 1 }

-- (-1) is reserved for storing the old mark pointer
lastArgument :: Address
lastArgument = (-2)
prevArgument :: Address -> Address
prevArgument = pred

-- Stores information about arguments in the state
assignCurrentFunction :: IRFunc [BasicBlock] -> TranslationState -> TranslationState
assignCurrentFunction (Func _ args _ _) o = o
	{ temporaryLocations = inserts $ temporaryLocations o
	, stackPtr = 0 }
	where
		-- TODO: figure out real size of an argument
		nArgs = length args
		locs = map (,Argument) [lastArgument - nArgs + 1 ..]
		ts = zip (map snd args) locs
		fs = map (uncurry insert) ts
		inserts = foldl (.) id fs

-- Pushes a persistent value on the stack
saveOnStack :: Temporary -> TranslationState -> TranslationState
saveOnStack t o = o { temporaryLocations = insert t (stackPtr o, Local) $ temporaryLocations o }

-- Emit a single instruction
out :: SSM.Instruction -> WriterT Output (State TranslationState) ()
out x = tell [x]

-- Returns relative position to MP for arguments,
-- absolute position form 0 for globals
-- If it couldn't be found, we asume it is a new temporary, and add it
getDataLocation :: Temporary -> State TranslationState (Maybe (Address, DataKind))
getDataLocation t = Map.lookup t <$> temporaryLocations <$> get

-- Pushes an temporary on the stack
loadData :: (Address, DataKind) -> WriterT Output (State TranslationState) ()
loadData (location, Global)	= out (SSM.LoadRegister SSM.R5) >> out (SSM.LoadViaAddress location) >> modify increaseStackPtr
loadData (location, Argument)	= out (SSM.LoadLocal location) >> modify increaseStackPtr
loadData (location, Local)	= out (SSM.LoadLocal location) >> modify increaseStackPtr

-- Preamble for functions
functionStart :: Label -> WriterT Output (State TranslationState) ()
functionStart l = do
	out (SSM.Label l)
	out (SSM.LoadRegister SSM.MP)
	out (SSM.LoadRegisterFromRegister SSM.MP SSM.SP)


-- We use the Writer monad to automatically apply ++ everywhere, and the State monad for information about stack/temporaries
class Translate a where
	translate :: a -> WriterT Output (State TranslationState) ()

instance Translate (Program [BasicBlock]) where
	translate (fs, gs) = do
		out (SSM.LoadRegisterFromRegister SSM.R5 SSM.SP)
		translate gs
		out (SSM.BranchToSubroutine "main")
		out (SSM.BranchAlways "end")
		printFunc
		translate fs
		out (SSM.Label "end")
		out SSM.Halt

instance Translate [IRGlob] where
	translate gs = do
		-- Push globals on stack
		forM_ gs (\(Glob n t _) -> do
			out (SSM.LoadConstant 0)
			lift $ modify increaseStackPtr
			s <- lift $ get
			let tlocs2 = insert n ((stackPtr s), Global) (temporaryLocations s)
			lift $ put s { temporaryLocations = tlocs2 })
		-- Initialise them
		forM_ gs (\(Glob _ _ label) -> out (SSM.BranchToSubroutine label))

instance Translate [IRFunc [BasicBlock]] where
	translate fs = mapM_ translate fs

-- TODO: arguments/returning/etc
instance Translate (IRFunc [BasicBlock]) where
	translate f@(Func l _ body _) = do
		lift . modify $ assignCurrentFunction f
		functionStart l
		translate body

instance Translate [BasicBlock] where
	translate bbs = mapM_ translate bbs

instance Translate BasicBlock where
	translate bb = mapM_ translate bb

instance Translate IRStmt where
	translate (Move (Data _ t) e2) = do
		thing <- lift $ getDataLocation t
		case thing of
			Nothing -> do
				translate e2 -- It simply lives on the stack
				lift . modify $ saveOnStack t
			Just (y, Global) -> do
				translate e2
				out (SSM.LoadRegister SSM.R5)
				out (SSM.StoreViaAddress y)
				lift $ modify decreaseStackPtr
			Just (y, _) -> do
				translate e2
				out (SSM.StoreLocal y)
				lift $ modify decreaseStackPtr
	translate (Move (Temp _ t) e2) = do
		translate e2
		lift $ modify increaseStackPtr
	translate (Expression e) = translate e
	translate (Jump label) = out (SSM.BranchAlways label)
	translate (CJump e tl fl) = do
		translate e
		out (SSM.BranchOnTrue tl)
		out (SSM.BranchAlways fl)
	translate (Seq s1 s2) = do
		translate s1
		translate s2
	translate (Ret (Just e)) = do
		translate e
		out (SSM.StoreRegister SSM.RR)
		-- This cleans up the stack
		out (SSM.LoadRegisterFromRegister SSM.SP SSM.MP)
		out (SSM.StoreRegister SSM.MP)
		out (SSM.Return)
	translate (Ret Nothing) = do
		out (SSM.LoadRegisterFromRegister SSM.SP SSM.MP)
		out (SSM.StoreRegister SSM.MP)
		out (SSM.Return)
	translate (Label l) = out (SSM.Label l)
	translate (Nop) = out (SSM.NoOperation)

instance Translate IRExpr where
	translate (Const _ n) = do
		out (SSM.LoadConstant n)
		lift $ modify increaseStackPtr
	translate (Data _ t) = do
		thing <- lift $ getDataLocation t
		case thing of
			Nothing -> error "COMPILER BUG: using a non-existing value"
			Just x -> loadData x
	translate (Temp _ t) = return () -- It lives on the stack?
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
		out (SSM.AdjustStack (negate $ length args))
		-- TODO: not always load the RR, maybe it doesnt do any harm
		out (SSM.LoadRegister SSM.RR)
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
	translate (AST.Equals _)		= out SSM.Equal
	translate (AST.LesserThan _)		= out SSM.LesserThan
	translate (AST.GreaterThan _)		= out SSM.GreaterThan
	translate (AST.LesserEqualThan _)	= out SSM.LesserEqual
	translate (AST.GreaterEqualThan _)	= out SSM.GreaterEqual
	translate (AST.Nequals _)		= out SSM.NotEqual

instance Translate IRUOps where
	translate (AST.Not _)			= out SSM.Not
	translate (AST.Negative _)		= out SSM.Negation


-- Basics for a ssm program
printFunc :: WriterT Output (State TranslationState) ()
printFunc = do
	functionStart "print"
	out (SSM.LoadLocal lastArgument)
	out (SSM.Trap 0)
	out (SSM.StoreRegister SSM.MP)
	out (SSM.Return)

-- Tie it together
total :: Program [BasicBlock] -> WriterT Output (State TranslationState) ()
total p = translate p

irToSSM :: Program [BasicBlock] -> SSM.Program
irToSSM = flip evalState emptyState . execWriterT . total
