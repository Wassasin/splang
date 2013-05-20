{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, TupleSections #-}

module IRtoSSM where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldr, foldl, map)
import Utils

-- We currently need this, because IR binops are AST binops
import qualified AST
import IR
import qualified SSM


-- Obvious output
type Output = SSM.Program

-- Can be absolute or relative addresses
type Address = Int
type Size = Int

data DataKind
	= Global
	| Argument
	| Local

sizeOf :: Type -> Size
sizeOf Bool = 1
sizeOf Int = 1
sizeOf (Pair t1 t2) = sizeOf t1 + sizeOf t2
sizeOf (ListPtr _) = 1
sizeOf ListAbstractEmpty = 1

sizeOfm :: Maybe Type -> Size
sizeOfm (Just t) = sizeOf t
sizeOfm Nothing = 0

-- Information about the stack and temporaries
-- temporaryLocations gives information about the temps
-- stackPtr is the stack pointer relative to the MP
-- return location is relative to the frame ptr
data TranslationState = TranslationState
	{ temporaryLocations :: Map Temporary (Address, DataKind)
	, stackPtr :: Address
	, returnLocation :: Address
	, returnSize :: Size }

emptyState = TranslationState
	{ temporaryLocations = empty
	, stackPtr = 0
	, returnLocation = 0
	, returnSize = 0 }

-- Some useful thingies
increaseStackPtrBy n o = o { stackPtr = n + (stackPtr o) }
increaseStackPtr = increaseStackPtrBy 1
decreaseStackPtrBy n o = o { stackPtr = (stackPtr o) - n }
decreaseStackPtr = decreaseStackPtrBy 1

-- (-1) is reserved for storing the old mark pointer
lastArgument :: Address
lastArgument = (-2)
prevArgument :: Address -> Address
prevArgument = pred

-- Stores information about arguments in the state
assignCurrentFunction :: IRFunc [BasicBlock] -> TranslationState -> TranslationState
assignCurrentFunction (Func _ args _ ty) o = o
	{ temporaryLocations = inserts $ temporaryLocations o
	, stackPtr = 0
	, returnLocation = retLoc
	, returnSize = retSize }
	where
		argSizes = scanl (flip $ (+) . sizeOf) 0 $ map fst args
		argsSize = last argSizes
		locs = map (\s -> (lastArgument - argsSize + 1 + s, Argument)) argSizes
		ts = zip (map snd args) locs
		fs = map (uncurry insert) ts
		inserts = foldl (.) id fs

		retSize = sizeOfm ty
		retLoc = lastArgument - argsSize - retSize + 1

addToLocations :: Temporary -> (Address, DataKind) -> TranslationState -> TranslationState
addToLocations t x o = o { temporaryLocations = insert t x $ temporaryLocations o }

-- Pushes a persistent value on the stack
saveOnStack :: Temporary -> Size -> TranslationState -> TranslationState
saveOnStack t n o = o { temporaryLocations = insert t (stackPtr o - n + 1, Local) $ temporaryLocations o }

-- Emit a single instruction
out :: SSM.Instruction -> WriterT Output (State TranslationState) ()
out x = tell [x]

-- Returns relative position to MP for arguments,
-- absolute position form 0 for globals
-- If it couldn't be found, we asume it is a new temporary, and add it
getDataLocation :: Temporary -> State TranslationState (Maybe (Address, DataKind))
getDataLocation t = Map.lookup t <$> temporaryLocations <$> get

-- Pushes an temporary on the stack
loadData :: (Address, DataKind) -> Size -> WriterT Output (State TranslationState) ()
loadData (location, Global) n	= out (SSM.LoadRegister SSM.R5) >> out (SSM.LoadMultipleViaAddress location n) >> modify (increaseStackPtrBy n)
loadData (location, _) n	= out (SSM.LoadMultipleLocal location n) >> modify (increaseStackPtrBy n)

-- Preamble for functions
functionStart :: Label -> WriterT Output (State TranslationState) ()
functionStart l = do
	out (SSM.Label l)
	out (SSM.LoadRegister SSM.MP)
	out (SSM.LoadRegisterFromRegister SSM.MP SSM.SP)

functionEnd :: WriterT Output (State TranslationState) ()
functionEnd = do		
	out (SSM.LoadRegisterFromRegister SSM.SP SSM.MP)
	out (SSM.StoreRegister SSM.MP)
	out (SSM.Return)

-- We use the Writer monad to automatically apply ++ everywhere, and the State monad for information about stack/temporaries
class Translate a where
	translate :: a -> WriterT Output (State TranslationState) ()

instance Translate (Program [BasicBlock]) where
	translate (fs, gs) = do
		-- Store current stack ptr for globals
		out (SSM.LoadRegisterFromRegister SSM.R5 SSM.SP)
		-- Initialise globals
		translate gs
		-- GO!
		out (SSM.BranchToSubroutine "main_v")
		out (SSM.BranchAlways "end")
		translate fs
		out (SSM.Label "end")
		out SSM.Halt

instance Translate [IRGlob] where
	translate gs = do
		-- Push globals on stack
		forM_ gs (\(Glob n t _) -> do
			-- zero initialized
			replicateM_ (sizeOf t) $ out (SSM.LoadConstant 0)
			s <- lift $ get
			lift . modify $ addToLocations n (1 + (stackPtr s), Global)
			lift . modify $ increaseStackPtrBy (sizeOf t))
		-- Initialise them
		forM_ gs (\(Glob _ _ label) -> out (SSM.BranchToSubroutine label))

instance Translate [IRFunc [BasicBlock]] where
	translate fs = mapM_ translate fs

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
	translate (Move (Data ty n) e2) = do
		thing <- lift $ getDataLocation n
		case thing of
			-- New local variable
			Nothing -> do
				translate e2
				lift . modify $ saveOnStack n (sizeOf ty)
			-- Global variable
			Just (y, Global) -> do
				translate e2
				out (SSM.LoadRegister SSM.R5)
				out (SSM.StoreMultipleViaAddress y (sizeOf ty))
				lift . modify . decreaseStackPtrBy $ sizeOf ty
			-- Local variable or argument
			Just (y, _) -> do
				translate e2
				out (SSM.StoreMultipleLocal y (sizeOf ty))
				lift . modify . decreaseStackPtrBy $ sizeOf ty
	translate (Move (Temp ty n) e2) = do
		-- Temporary (not persistent)
		translate e2
		lift . modify $ saveOnStack n (sizeOf ty)
	translate (Expression e) = translate e
	translate (Jump label) = out (SSM.BranchAlways label)
	translate (CJump e tl fl) = do
		translate e
		out (SSM.BranchOnTrue tl)
		out (SSM.BranchAlways fl)
		lift $ modify decreaseStackPtr
	translate (Seq s1 s2) = do
		translate s1
		translate s2
	translate (Ret (Just e)) = do
		translate e
		retLoc <- lift $ returnLocation <$> get
		retSize <- lift $ returnSize <$> get
		out (SSM.StoreMultipleLocal retLoc retSize)
		functionEnd
	translate (Ret Nothing) = functionEnd
	translate (Label l) = out (SSM.Label l)
	translate (Nop) = out (SSM.NoOperation)

instance Translate IRExpr where
	translate (Const _ n) = do
		out (SSM.LoadConstant n)
		lift $ modify increaseStackPtr
	translate (Data ty n) = do
		thing <- lift $ getDataLocation n
		case thing of
			Nothing -> error "COMPILER BUG: using a non-existing value"
			Just x -> loadData x (sizeOf ty)
	translate (Temp ty n) = do
		sp <- lift $ stackPtr <$> get
		Just (loc, _) <- lift $ getDataLocation n
		let diff = loc - sp
		if diff == 0
			then return ()
			else out (SSM.LoadMultipleFromStack diff (sizeOf ty))
	translate (Binop _ e1 bop e2) = do
		translate e1
		translate e2
		translate bop
		lift $ modify decreaseStackPtr
	translate (Unop _ uop e) = do
		translate e
		translate uop
	translate (Call ty label args) = do
		-- allocate space for return value
		replicateM_ (sizeOfm ty) $ out (SSM.LoadConstant 0)
		lift $ modify (increaseStackPtrBy (sizeOfm ty))
		-- push arguments (normal order)
		mapM translate args
		-- call
		out (SSM.BranchToSubroutine label)
		-- clean up arguments
		let argSize = foldr ((+) . sizeOfm . typeOf) 0 args
		out (SSM.AdjustStack (-argSize))
		lift $ modify (decreaseStackPtrBy argSize)
		-- result is stored in the right spot by the callee
	translate (Builtin _ (IR.MakePair e1 e2)) = do
		-- Pairs have a flat layout
		translate e1
		translate e2
	translate (Builtin _ (IR.First e)) = do
		-- Construct the pair, discard the second part
		translate e
		let (Pair t1 t2) = guardJust "COMPILER BUG (IR->SSM): applying fst to a non-tuple in codegen" $ typeOf e
		out (SSM.AdjustStack (negate $ sizeOf t2))
		lift $ modify (decreaseStackPtrBy (sizeOf t2))
	translate (Builtin _ (IR.Second e)) = do
		-- Construct the pair, copy the second part to current place in stack
		translate e
		let t@(Pair t1 t2) = guardJust "COMPILER BUG (IR->SSM): applying fst to a non-tuple in codegen" $ typeOf e
		out (SSM.StoreMultipleIntoStack (1 - (sizeOf t)) (sizeOf t2))
		out (SSM.AdjustStack (sizeOf t2 - 1))
		lift $ modify (decreaseStackPtrBy (sizeOf t1))
	translate (Builtin _ (IR.Print e)) = do
		-- TODO: Print more for other types?
		translate e
		out (SSM.Trap 0)
	translate (Builtin _ (IR.Cons e1 e2)) = do
		-- First store the pointer to the next value, then the data, this is nice, because StoreMultipleHeap gives an address
		let fJust = guardJust "COMPILER BUG (IR->SSM): value in cons has no type"
		translate e2
		translate e1
		out (SSM.StoreMultipleHeap ((sizeOfm $ typeOf e2) + (sizeOfm $ typeOf e1)))
		lift $ modify (decreaseStackPtrBy (sizeOfm $ typeOf e1)) -- NOTE: the address will be overwritten, and has the same size
	translate (Builtin _ (IR.IsEmpty e)) = do
		translate e
		out (SSM.LoadConstant 0)
		out SSM.Equal
	translate (Builtin (Just t) (IR.Tail e)) = case t of
			ListAbstractEmpty -> do
				translate e
				out (SSM.LoadMultipleHeap 0 (sizeOf t))
				lift $ modify (increaseStackPtrBy (sizeOf t - 1))
			ListPtr et -> do
				translate e
				out (SSM.LoadMultipleHeap 0 (sizeOf t + sizeOf et))
				out (SSM.AdjustStack (negate (sizeOf et)))
				lift $ modify (increaseStackPtrBy (sizeOf t - 1))
	translate (Builtin (Just et) (IR.Head e)) = do
		let t = ListPtr et
		let size = (sizeOf t + sizeOf et)
		translate e
		out (SSM.LoadMultipleHeap 0 size)
		out (SSM.StoreMultipleIntoStack (1 - size) (sizeOf et))
		out (SSM.AdjustStack (sizeOf et - 1))
		lift $ modify (increaseStackPtrBy (sizeOf et - 1))

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

-- Tie it together
total :: Program [BasicBlock] -> WriterT Output (State TranslationState) ()
total p = translate p

irToSSM :: Program [BasicBlock] -> SSM.Program
irToSSM = flip evalState emptyState . execWriterT . total
