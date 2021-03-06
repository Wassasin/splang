{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveFunctor #-}

module IR where

import Prelude hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Applicative((<$>))
import Data.Traversable

-- cabal install derive
import Data.DeriveTH

import Utils
import qualified AST

type Label		= String
type Value		= Int
type Temporary		= Int

data Type		= Bool | Int | Pair Type Type | ListPtr Type | ListAbstractEmpty
	deriving (Eq, Ord, Show)

data IRFunc a = Func Label [(Type, Temporary)] a (Maybe Type)
	deriving (Functor, Eq, Ord, Show)

-- Label is the initialisation function
data IRGlob = Glob Temporary Type Label
data IRDecls = ExternFun Label [Type] (Maybe Type)

type IRBOps = AST.BinaryOperator ()
type IRUOps = AST.UnaryOperator ()

-- Types will be used for example to store intermediate results
data IRExpr
	= Const Type Value			-- A constant
	| Temp Type Temporary			-- Temporary (infi many)
	| Data Type Temporary			-- Persistent temporary, ie: globals, varargs, locals, but not subexpressions
	| Binop Type IRExpr IRBOps IRExpr	-- Binary Operation
	| Unop Type IRUOps IRExpr		-- Unary Operation
	| Call (Maybe Type) Label [IRExpr]	-- Call to address (first expr) with arguments (list of exprs)
	| Builtin (Maybe Type) IRBuiltin	-- Builtin function
	deriving (Eq, Ord, Show)

-- TODO: List manipulations, or maybe even malloc things
data IRBuiltin
	= MakePair IRExpr IRExpr
	| First IRExpr
	| Second IRExpr
	| Cons IRExpr IRExpr
	| IsEmpty IRExpr
	| Tail IRExpr
	| Head IRExpr
	| Print IRExpr
	deriving (Eq, Ord, Show)

data IRStmt
	= Move IRExpr IRExpr		-- move dest <- source
	| Expression IRExpr		-- evaluate expression
	| Jump Label			-- jump to label
	| CJump IRExpr Label Label 	-- evaluate the bool expressions, jump to either of the labels
	| Seq IRStmt IRStmt		-- combine statements with ;
	| Ret (Maybe IRExpr)		-- returns from function
	| Label Label 			-- code label
	| Nop				-- might be handy
	deriving (Eq, Ord, Show)

type BasicBlock = [IRStmt]
type Program a = ([IRFunc a], [IRGlob], [IRDecls])

-- Derive the isConstructor functions :)
$( derive makeIs ''IRExpr)
$( derive makeIs ''IRBuiltin)
$( derive makeIs ''IRStmt)

-- Bit more general
isaJump :: IRStmt -> Bool
isaJump x = isJump x || isCJump x || isRet x

isaLabel :: IRStmt -> Bool
isaLabel x = isLabel x

-- Call expressions might return nothing
-- TODO: everything should have a type, we need this for eg returning tuples etc
typeOf :: IRExpr -> Maybe Type
typeOf (Const t _)	= Just t
typeOf (Temp t _)	= Just t
typeOf (Data t _)	= Just t
typeOf (Binop t _ _ _)	= Just t
typeOf (Unop t _ _)	= Just t
typeOf (Call t _ _)	= t
typeOf (Builtin t _)	= t

-- TODO: Make a more sophisticated algorithm
class SideEffectSensitive a where
	sideEffectSensitive :: a -> Bool

instance SideEffectSensitive IRExpr where
	sideEffectSensitive (Const _ _) = False
	sideEffectSensitive (Temp _ _) = False
	sideEffectSensitive (Data _ _) = True
	sideEffectSensitive (Binop _ e1 _ e2) = sideEffectSensitive e1 || sideEffectSensitive e2
	sideEffectSensitive (Unop _ _ e1) = sideEffectSensitive e1
	sideEffectSensitive (Builtin _ b) = sideEffectSensitive b
	sideEffectSensitive (Call _ _ _) = True

instance SideEffectSensitive IRBuiltin where
	sideEffectSensitive (MakePair e1 e2) = sideEffectSensitive e1 || sideEffectSensitive e2
	sideEffectSensitive (First e) = sideEffectSensitive e
	sideEffectSensitive (Second e) = sideEffectSensitive e
	sideEffectSensitive (Cons e1 e2) = sideEffectSensitive e1 || sideEffectSensitive e2
	sideEffectSensitive (IsEmpty e) = sideEffectSensitive e
	sideEffectSensitive (Tail e) = sideEffectSensitive e
	sideEffectSensitive (Head e) = sideEffectSensitive e
	sideEffectSensitive (Print e) = True

-- TODO: also take s into account
commute :: IRStmt -> IRExpr -> Bool
commute Nop _ = True
commute _ e = not $ sideEffectSensitive e

-- Keep track of the temporaries
type CanonicalizeState = Temporary

getFreshTemporary :: Type -> State CanonicalizeState IRExpr
getFreshTemporary typ = do
	t <- get
	put (t+1)
	return $ Temp typ t


-- Will move SEQ up, remove ESEQ
-- Return types means, first do the statement, then do the other thing (like (E)Seq)
class Canonicalize a where
	canonicalize :: a -> State CanonicalizeState (IRStmt, a)

-- In this case we don't return (Seq s1 s2) as in the slides, but (s1, s2), because that's the type ;)
instance Canonicalize IRStmt where
	canonicalize (Seq s1 s2) = do
		s1' <- uncurry Seq <$> canonicalize s1
		s2' <- uncurry Seq <$> canonicalize s2
		return (s1', s2')
	canonicalize (Move dst@(Data _ _) src) = do
		(s, src) <- canonicalize src
		return (s, Move dst src)
	canonicalize (Move dst@(Temp _ _) src) = do
		(s, src) <- canonicalize src
		return (s, Move dst src)
	canonicalize (Move dst src) = do
		(s, [dst, src]) <- canonicalize [dst, src]
		return (s, Move dst src)
	canonicalize (Expression e) = do
		(s, e') <- canonicalize e
		return (s, Expression e')
	canonicalize (CJump e tl fl) = do
		(s, e) <- canonicalize e
		return (s, CJump e tl fl)
	-- Label, Jump
	canonicalize x = return (Nop, x)

instance Canonicalize IRExpr where
	canonicalize (Binop t e1 b e2) = do
		(s, [e1', e2']) <- canonicalize [e1, e2]
		return (s, Binop t e1' b e2')
	canonicalize (Unop t uop e) = fmap (Unop t uop) <$> canonicalize e
	canonicalize (Call t f l) = fmap (Call t f) <$> canonicalize l
	canonicalize (Builtin t b) = fmap (Builtin t) <$> canonicalize b
	-- Const, Name, Temp, Data
	canonicalize x = return (Nop, x)

instance Canonicalize IRBuiltin where
	canonicalize (MakePair e1 e2) = do
		(s, [e1, e2]) <- canonicalize [e1, e2]
		return (s, MakePair e1 e2)
	canonicalize (First e)	= fmap First <$> canonicalize e
	canonicalize (Second e)	= fmap Second <$> canonicalize e
	canonicalize (Print e)	= fmap Print <$> canonicalize e
	canonicalize (Cons e1 e2) = do
		(s, [e1, e2]) <- canonicalize [e1, e2]
		return (s, Cons e1 e2)
	canonicalize (IsEmpty e)= fmap IsEmpty <$> canonicalize e
	canonicalize (Tail e)	= fmap Tail <$> canonicalize e
	canonicalize (Head e)	= fmap Head <$> canonicalize e

instance Canonicalize [IRExpr] where
	canonicalize [] = return (Nop, [])
	-- foldM is from left to right, so we reverse and flip
	canonicalize l = foldM (flip combine) (Nop, []) =<< l'
		where
			l' = mapM canonicalize (reverse l)
			combine (s, e) (s1, es) = if commute s1 e
				then do
					return (Seq s s1, e:es)
				else do
					t <- getFreshTemporary (guardJust "COMPILER BUG (IR): trying to store a void-value" (typeOf e))
					let (s', e') = (Seq (Move t e) s1, t)
					return (Seq s s', e':es)


-- Will remove SEQs, so that it'll be linear, note SEQs should be on top
flatten :: IRStmt -> [IRStmt]
flatten (Seq l r) = flatten l ++ flatten r
flatten (Nop) = []
flatten x = [x]


-- Such that the first one is a label, last one is a jump
type PartitionState = [Label]

freshLabel :: PartitionState -> Label
freshLabel [] = "start"
freshLabel l = "fresh" ++ show (length l)

genBegin :: (Functor m, Monad m) => Bool -> StateT PartitionState m [IRStmt]
genBegin True = return []
genBegin False = do
	freshLabel <- freshLabel <$> get
	modify (freshLabel:)
	return [Label freshLabel]

errorLabel = "_DEADLABEL" -- special label that should not exist in eventual code

partitionBBs :: (Functor m, Monad m) => [IRStmt] -> StateT PartitionState m [BasicBlock]
partitionBBs []		= return []
partitionBBs (x:xs)	= do
	begin <- genBegin (isaLabel (head ls))
	rest <- partitionBBs rs
	return $ [begin ++ ls ++ end] ++ rest
	where
		span (l1, [])		= (l1, [])
		span (l1, (y:ys))
			| isaLabel y	= (l1, y:ys)
			| isaJump y 	= (l1 ++ [y], ys)
			| otherwise	= span (l1 ++ [y], ys)
		(ls, rs)
			| isaJump x 	= ([x], xs)
			| otherwise	= span ([x], xs)
		nextLabel = case rs of
			[]		-> errorLabel
			(Label str:_)	-> str
			_		-> error "COMPILER BUG: The impossible happend (impossible pattern match)"
		end
			| isaJump (last ls)	= []
			| otherwise		= [Jump nextLabel]

startWith :: s -> State s a -> a
startWith = flip evalState

startWithT :: Monad m => s -> StateT s m a -> m a
startWithT = flip evalStateT

-- Combining all the above, state shouldnt start at 0, because there may already be temporaries
linearizeStmt :: IRStmt -> StateT PartitionState (State CanonicalizeState) [BasicBlock]
linearizeStmt s = do
	(s1, s2) <- lift (canonicalize s)
	partitionBBs . flatten $ Seq s1 s2

linearizeFunc :: IRFunc IRStmt -> StateT PartitionState (State CanonicalizeState) (IRFunc [BasicBlock])
linearizeFunc (Func a b body c) = do
	body <- linearizeStmt body
	return $ Func a b body c

linearize :: IR.Program IRStmt -> IR.Program [BasicBlock]
linearize (fs, gs, ds) = (startWith 0 . startWithT [] . mapM linearizeFunc $ fs, gs, ds)

-- TODO: Analyse traces and remove redundant labels

-- Output stuff
printIR :: Program IRStmt -> IO ()
printIR (fs, _, _) = mapM_ (\(Func l args body t) -> do
	putStrLn $ l ++ show args ++ show t
	print body
	putStrLn "") $ fs

printCanonicalizedIR :: Program [BasicBlock] -> IO ()
printCanonicalizedIR (fs, _, _) = mapM_ (\(Func l args body t) -> do
	putStrLn $ l ++ show args ++ show t
	forM_ body (\bb -> forM_ bb (\s -> putStrLn $ show s ++ ";") >> putStrLn "------")
	putStrLn "") $ fs
