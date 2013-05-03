{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveFunctor #-}

module IR where

import Control.Monad.State
import Control.Applicative((<$>))

-- cabal install derive
import Data.DeriveTH

import qualified AST

type Label		= String
type Value		= Int
type Temporary		= Int

data Type		= Bool | Int | Pair Type Type | ListPtr Type
	deriving (Eq, Ord, Show)

data IRFunc a = Func Label [(Type, Temporary)] a (Maybe Type)
	deriving (Functor, Eq, Ord, Show)

type IRBOps = AST.BinaryOperator ()
type IRUOps = AST.UnaryOperator ()

-- TODO: Constants for all types
data IRExpr
	= Const Type Value		-- A constant
	| Temp Type Temporary		-- Temporary (infi many)
	| Binop IRExpr IRBOps IRExpr	-- Binary Operation
	| Unop IRUOps IRExpr		-- Unary Operation
	| Mem IRExpr			-- Expression which gives an address
	| Call Label [IRExpr]		-- Call to address (first expr) with arguments (list of exprs)
	| Eseq IRStmt IRExpr		-- ???
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

-- Derive the isConstructor functions :)
$( derive makeIs ''IRExpr)
$( derive makeIs ''IRStmt)

-- Bit more general
isaJump :: IRStmt -> Bool
isaJump x = isJump x || isCJump x || isRet x

isaLabel :: IRStmt -> Bool
isaLabel x = isLabel x

-- TODO: Make a more sophisticated algorithm
sideEffectSensitive :: IRExpr -> Bool
sideEffectSensitive (Const _ _) = False
sideEffectSensitive (Temp _ _) = True
sideEffectSensitive (Binop e1 _ e2) = sideEffectSensitive e1 || sideEffectSensitive e2
sideEffectSensitive (Unop _ e1) = sideEffectSensitive e1
sideEffectSensitive (Mem e1) = sideEffectSensitive e1
sideEffectSensitive (Call _ _) = True
sideEffectSensitive (Eseq _ _) = True

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
	canonicalize (Move dst@(Temp _ _) src) = do
		(s, src) <- canonicalize src
		return (s, Move dst src)
	-- Figure out whether this is always sane semantics
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
	canonicalize (Eseq s e) = do
		s' <- uncurry Seq <$> canonicalize s
		(s2, e') <- canonicalize e
		return ((Seq s' s2), e')
	canonicalize (Binop e1 b e2) = do
		(s, [e1', e2']) <- canonicalize [e1, e2]
		return (s, Binop e1' b e2')
	canonicalize (Mem e) = do
		(s, e') <- canonicalize e
		return (s, Mem e')
	canonicalize (Call f l) = do
		(s, l') <- canonicalize (l)
		return (s, Call f l')
	-- Const, Name, Temp
	canonicalize x = return (Nop, x)

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
					t <- getFreshTemporary Bool -- TODO does not always return Bool
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

genBegin :: Bool -> State PartitionState [IRStmt]
genBegin True = return []
genBegin False = do
	freshLabel <- freshLabel <$> get
	modify (freshLabel:)
	return [Label freshLabel]

-- TODO: also keep "end" in state
partitionBBs :: [IRStmt] -> State PartitionState [BasicBlock]
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
			[]		-> "end"
			(Label str:_)	-> str
			_		-> error "COMPILER BUG: The impossible happend (impossible pattern match)"
		end
			| isaJump (last ls)	= []
			| otherwise		= [Jump nextLabel]

startWith :: s -> State s a -> a
startWith = flip evalState

-- Combining all the above, state shouldnt start at 0, because there may already be temporaries
-- TODO: if partitionBBS uses State monad, keep the state
linearize :: IRStmt -> [BasicBlock]
linearize = startWith [] . partitionBBs . flatten . uncurry Seq . startWith 0 . canonicalize

-- TODO: Analyse traces and remove redundant labels

-- For testing/debugging
printBBs :: [BasicBlock] -> IO ()
printBBs = putStr . unlines . fmap (foldr (\x y -> x ++ " ;; " ++ y) "") . fmap (fmap show)

c = Const Int 5
j = Jump "bla"
l = Label "poo"
m = Move c c
