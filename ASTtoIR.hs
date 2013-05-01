{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module ASTtoIR where

import Data.Maybe
import Data.Traversable as Trav
import Control.Monad.State
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldl)

import qualified AST
import qualified IR

data TranslationState = TranslationState { labels :: [IR.Label], identifierTemporaries :: Map AST.IdentID IR.IRExpr }
	deriving (Eq, Ord, Show)

addLabel :: IR.Label -> TranslationState -> TranslationState
addLabel l s = s { labels = l:(labels s) }

genLabel :: String -> State TranslationState IR.Label
genLabel str = do
	ls <- labels <$> get
	let str2 = str ++ "_" ++ show (length ls)
	modify (addLabel str2)
	return str2

-- TODO: lookup
getFunctionLabel :: (AST.Identifier a) -> State TranslationState IR.Label
getFunctionLabel (AST.Identifier str _ _) = return str

class Translate a b | a -> b where
	translate :: a -> State TranslationState b

instance Translate (AST.Stmt a) IR.IRStmt where
	translate (AST.Expr e _) = do
		e <- translate e
		return $ IR.Expression e
	translate (AST.Scope l _) = do
		l <- Trav.mapM translate l
		return $ foldl IR.Seq IR.Nop l
	translate (AST.If e s m) = translate (AST.IfElse e s (AST.Scope [] m) m)
	translate (AST.IfElse e s1 s2 _) = do
		e <- translate e
		s1 <- translate s1
		s2 <- translate s2
		tl <- genLabel "if_true"
		fl <- genLabel "if_false"
		fi <- genLabel "if_end"
		let ss = [IR.CJump e tl fl, IR.Label tl, s1, IR.Jump fi, IR.Label fl, s2, IR.Label fi]
		return $ foldl IR.Seq IR.Nop ss
	translate (AST.While e s _) = do
		e <- translate e
		s <- translate s
		test <- genLabel "while_test"
		true <- genLabel "while_true"
		wend <- genLabel "while_end"
		let ss = [IR.Label test, IR.CJump e true wend, IR.Label true, s, IR.Jump test, IR.Label wend]
		return $ foldl IR.Seq IR.Nop ss
	translate (AST.Assignment (AST.Identifier _ n _) e _) = do
		e <- translate e
		t <- fromJust <$> Map.lookup (fromJust n) <$> identifierTemporaries <$> get
		return $ IR.Move t e
	translate (AST.Return me _) = do
		e <- Trav.mapM translate me
		return $ IR.Ret e

-- TODO: do something when the function is polymorphic
-- TODO: pair
-- TODO: get right type of ListPtr
instance Translate (AST.Expr a) IR.IRExpr where
	translate (AST.Var (AST.Identifier _ n _) _) = fromJust <$> Map.lookup (fromJust n) <$> identifierTemporaries <$> get
	translate (AST.Binop e1 bop e2 _) = do
		e1 <- translate e1
		bop <- translate bop
		e2 <- translate e2
		return $ IR.Binop e1 bop e2
	translate (AST.Unop uop e _) = do
		uop <- translate uop
		e <- translate e
		return $ IR.Unop uop e
	translate (AST.Kint n _) = return $ IR.Const IR.Int n
	translate (AST.Kbool n _) = return $ IR.Const IR.Bool 0
	translate (AST.FunCall ident l _) = do
		flabel <- getFunctionLabel ident
		l <- Trav.mapM translate l
		return $ IR.Call flabel l
	translate (AST.Pair e1 e2 _) = undefined
	translate (AST.Nil _) = return $ IR.Const (IR.ListPtr IR.Int) 0


instance Translate (AST.BinaryOperator a) IR.IRBOps where
	translate x = return $ fmap (const ()) x

instance Translate (AST.UnaryOperator a) IR.IRUOps where
	translate x = return $ fmap (const ()) x


-- For Debugging/Testing
emptyState = TranslationState { labels = [], identifierTemporaries = empty }
f = IR.printBBs . IR.linearize . flip evalState emptyState . translate :: (AST.Stmt a) -> IO ()
b = AST.Scope [AST.Expr (AST.FunCall (AST.Identifier "print" Nothing ()) [AST.Kint 5 ()] ()) (), AST.Return Nothing ()] ()
e = AST.Binop (AST.Kint 1()) (AST.Equals ()) (AST.Kint 3()) ()
s = AST.Scope [AST.While e b (), AST.While e b (), AST.Return Nothing ()] ()
