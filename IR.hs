{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module IR where

-- cabal install derive
import Data.DeriveTH
import Data.Derive.Is

type Label		= String
type Value		= Int
type Temporary		= Int
type Type		= ()

data IRBOps
	= Addition
	| Substraction
	| Multiplication
	-- etc...
	deriving (Eq, Ord, Show)

data IRExpr
	= Const Type Value		-- A constant
	| Name Type Label		-- A code label
	| Temp Type Temporary		-- Temporary (infi many)
	| Binop IRBOps IRExpr IRExpr	-- Binary Operation
	| Mem IRExpr			-- Expression which gives an address
	| Call IRExpr [IRExpr]		-- Call to address (first expr) with arguments (list of exprs)
	| Eseq IRStmt IRExpr		-- ???
	deriving (Eq, Ord, Show)

data IRStmt
	= Move IRExpr IRExpr		-- move dest <- source
	| Expression IRExpr		-- evaluate expression
	| Jump IRExpr [Label]		-- evaluate expression and jump to corresponding label?
	| CJump IRBOps IRExpr IRExpr Label Label -- evaluate two expressions, compare, jump to either of the labels
	| Seq IRStmt IRStmt		-- combine statements with ;
	| Label Label 			-- code label
	| Nop				-- might be handy
	deriving (Eq, Ord, Show)

-- Derive the isConstructor functions :)
$( derive makeIs ''IRBOps)
$( derive makeIs ''IRExpr)
$( derive makeIs ''IRStmt)

-- Bit more general
isaJump :: IRStmt -> Bool
isaJump x = isJump x || isCJump x

isaLabel :: IRStmt -> Bool
isaLabel x = isLabel x


-- We can make some kind of monad structure here, for now, the temporary is constant :D
getFreshTemporary = Temp () 0


-- Will move SEQ up, remove ESEQ
-- Return types means, first do the statement, then do the other thing (like (E)Seq)
class Canonicalize a where
	canonicalize :: a -> (IRStmt, a)

-- In this case we don't return (Seq s1 s2) as in the slides, but (s1, s2), because that's the type ;)
-- I think this might be written donw more nicely (it's just one big pattern), but hey, this works ;)
instance Canonicalize IRStmt where
	canonicalize (Seq s1 s2) = (s1', s2')
		where s1' = uncurry Seq $ canonicalize s1
		      s2' = uncurry Seq $ canonicalize s2
	canonicalize (Move dst src) = (s, Move dst' src')
		where (s, [dst',src']) = canonicalize [dst, src]
	canonicalize (Expression e) = (s, Expression e')
		where (s, e') = canonicalize e
	canonicalize (Jump e l) = (s, Jump e' l)
		where (s, e') = canonicalize e
	canonicalize (CJump op e1 e2 tl fl) = (s, CJump op e1' e2' tl fl)
		where (s, [e1',e2']) = canonicalize [e1,e2]
	-- Label
	canonicalize x = (Nop, x)

instance Canonicalize IRExpr where
	canonicalize (Eseq s e) = ((Seq s' s2), e')
		where s' = uncurry Seq $ canonicalize s
		      (s2, e') = canonicalize e
	canonicalize (Binop b e1 e2) = (s, Binop b e1' e2')
		where (s, [e1', e2']) = canonicalize [e1, e2]
	canonicalize (Mem e) = (s, Mem e')
		where (s, e') = canonicalize e
	canonicalize (Call f l) = (s, Call f' l')
		where (s, f':l') = canonicalize (f:l)
	-- Const, Name, Temp
	canonicalize x = (Nop, x)

instance Canonicalize [IRExpr] where
	-- TODO: Make _fresh_ temporaries!
	canonicalize [] = (Nop, [])
	canonicalize l = foldr combine (Nop, []) l' 
		where
			l' = fmap canonicalize l
			-- TODO: this can be optimized if s1 and e commute
			combine (s, e) (s1, es) = (Seq s s', e':es)
				where (s', e') = (Seq (Move t e) s1, t)
				      t = getFreshTemporary


-- Will remove SEQs, so that it'll be linear, note SEQs should be on top
flatten :: IRStmt -> [IRStmt]
flatten (Seq l r) = flatten l ++ flatten r
flatten (Nop) = []
flatten x = [x]


-- Such that the first one is a label, last one is a jump
type BasicBlock = [IRStmt]

genFreshLabel :: [Label] -> Label
genFreshLabel [] = "start"
genFreshLabel l = "fresh" ++ show (length l)

partitionBBs :: [Label] -> [IRStmt] -> [BasicBlock]
partitionBBs labels []		= []
partitionBBs labels (x:xs)	= [begin ++ ls ++ end] ++ partitionBBs labels2 rs
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
		freshLabel = genFreshLabel labels
		(labels2, begin)
			| isaLabel (head ls)	= (labels, [])
			| otherwise		= (freshLabel:labels, [Label freshLabel])
		end
			| isaJump (last ls)	= []
			| otherwise		= [Jump (Const () 0) [nextLabel]]


-- Combining all the above
linearize :: IRStmt -> [BasicBlock]
linearize = partitionBBs [] . flatten . uncurry Seq . canonicalize


-- For testing/debugging
printBBs :: [BasicBlock] -> IO ()
printBBs = putStr . unlines . fmap (foldr (\x y -> x ++ " ;; " ++ y) "") . fmap (fmap show)

c = Const () 5
j = Jump c ["bla"]
l = Label "poo"
m = Move c c
