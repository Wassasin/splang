{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module ASTtoIR where

import Data.Maybe
import Data.Traversable as Trav
import Control.Monad.State
import Control.Applicative((<$>))
import Data.Map as Map hiding (foldl, map)
import TypeInference (P3, P3Meta, inferredType)

import qualified Typing (MonoType(..))
import qualified AST
import qualified IR

data TranslationState = TranslationState { labelNumber :: Int, labels :: [IR.Label], identifierTemporaries :: Map AST.IdentID IR.IRExpr }
	deriving (Eq, Ord, Show)
emptyState = TranslationState { labelNumber = 0, labels = [], identifierTemporaries = empty }

addLabel :: IR.Label -> TranslationState -> TranslationState
addLabel l s = s { labels = l:(labels s) }

increaseLabelNumber :: TranslationState -> TranslationState
increaseLabelNumber s = s { labelNumber = 1 + (labelNumber s) }

addTemporary :: AST.IdentID -> IR.IRExpr -> TranslationState -> TranslationState
addTemporary ident temp s = s { identifierTemporaries = insert ident temp $ identifierTemporaries s }

-- Generates unique labels, given a set of distinct strings
genLabels :: [String] -> State TranslationState [IR.Label]
genLabels strs = do
	n <- labelNumber <$> get
	strs <- Trav.forM strs (\str -> do 
		let str2 = str ++ "_" ++ show (n)
		modify (addLabel str2)
		return str2)
	modify increaseLabelNumber
	return strs

-- TODO: lookup
getFunctionLabel :: (AST.Identifier a) -> State TranslationState IR.Label
getFunctionLabel (AST.Identifier str _ _) = return str

class Translate a b | a -> b where
	translate :: a -> State TranslationState b

instance Translate (P3 AST.Program) [IR.IRFunc IR.IRStmt] where
	translate (AST.Program decls _) = Trav.mapM translate decls

-- TODO: What to do with (global) variables???
-- TODO: Do something with function arguments
instance Translate (P3 AST.Decl) (IR.IRFunc IR.IRStmt) where
	translate (AST.VarDecl _ (AST.Identifier str mn _) e m) = do
		t <- translate (fromJust $ inferredType m)
		e <- translate e
		let n = fromJust mn
		let d = IR.Data t n
		modify (addTemporary n d)
		return $ IR.Func ("init_globalvar_"++str) [] (IR.Expression e) Nothing
	translate (AST.FunDecl _ (AST.Identifier str _ _) args decls stmts _) = do
		Trav.forM args (\(_, AST.Identifier _ (Just n) _) -> modify $ addTemporary n (IR.Data IR.Int n))
		decls <- Trav.mapM translateLocalVarDecl decls
		stmts <- Trav.mapM translate stmts
		stmts <- return . foldl IR.Seq IR.Nop $ decls ++ stmts ++ [IR.Ret Nothing] -- FIXME: ugly hack to ensure functions always return
		return $ IR.Func str (map (\(_, AST.Identifier _ (Just n) _) -> (IR.Int, n)) args) stmts Nothing

translateLocalVarDecl :: P3 AST.Decl -> State TranslationState IR.IRStmt
translateLocalVarDecl (AST.FunDecl{}) = error "COMPILER BUG: function declarations cannot occur in function body."
translateLocalVarDecl (AST.VarDecl _ (AST.Identifier str mn _) e m) = do
	t <- translate (fromJust $ inferredType m)
	e <- translate e
	let identid = (fromJust mn)
	let d = IR.Data t identid
	modify (addTemporary identid d)
	return $ IR.Move d e

instance Translate (P3 AST.Stmt) IR.IRStmt where
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
		[tl, fl, fi] <- genLabels ["_if_true", "_if_false", "_if_end"]
		let ss = [IR.CJump e tl fl, IR.Label tl, s1, IR.Jump fi, IR.Label fl, s2, IR.Label fi]
		return $ foldl IR.Seq IR.Nop ss
	translate (AST.While e s _) = do
		e <- translate e
		s <- translate s
		[test, true, wend] <- genLabels ["_while_test", "_while_true", "_while_end"]
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
instance Translate (P3 AST.Expr) IR.IRExpr where
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
	translate (AST.Kbool True _) = return $ IR.Const IR.Bool (-1)
	translate (AST.Kbool False _) = return $ IR.Const IR.Bool 0
	translate (AST.FunCall ident l _) = do
		flabel <- getFunctionLabel ident
		l <- Trav.mapM translate l
		return $ IR.Call flabel l
	translate (AST.Pair _ _ _) = error "COMPILER BUG: Pairs not yet implemented"
	translate (AST.Nil _) = return $ IR.Const (IR.ListPtr IR.Int) 0


instance Translate (P3 AST.BinaryOperator) IR.IRBOps where
	translate x = return $ fmap (const ()) x

instance Translate (P3 AST.UnaryOperator) IR.IRUOps where
	translate x = return $ fmap (const ()) x

-- Typing
instance Translate (Typing.MonoType a) IR.Type where
	translate (Typing.Func _ _ _)	= error "COMPILER BUG: Can not directly translate a function to a datatype"
	translate (Typing.Pair x y _)	= do
		x <- translate x
		y <- translate y
		return $ IR.Pair x y
		
	translate (Typing.List x _)	= do
		x <- translate x
		return $ IR.ListPtr x
	translate (Typing.Free _ _)	= error "COMPILER BUG: Can not translate an abstract type to a concrete datatype"
	translate (Typing.Int _)	= return $ IR.Int
	translate (Typing.Bool _)	= return $ IR.Bool
	translate (Typing.Void _)	= error "COMPILER BUG: Can not translate a Void type to a concrete datatype"

translateProgram :: (P3 AST.Program) -> [IR.IRFunc IR.IRStmt]
translateProgram program = evalState (translate program) emptyState
