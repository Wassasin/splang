{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

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
		test <- genLabel "wile_test"
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

instance Translate (AST.Expr a) IR.IRExpr where
	translate x = undefined