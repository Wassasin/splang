{-# LANGUAGE TemplateHaskell #-}

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


-- Will move SEQ up, remove ESEQ
canonicallize :: IRStmt -> IRStmt
canonicallize x = x

-- Will remove SEQs, so that it'll be linear, note SEQs should be on top
flatten :: IRStmt -> [IRStmt]
flatten (Seq l r) = flatten l ++ flatten r
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
linearize = partitionBBs [] . flatten . canonicallize


-- For testing/debugging
printBBs :: [BasicBlock] -> IO ()
printBBs = putStr . unlines . fmap (foldl (\x y -> x ++ " ;; " ++ y) "") . fmap (fmap show)

c = Const () 5
j = Jump c ["bla"]
l = Label "poo"
m = Move c c
