{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module ASTWalker where

import Data.Traversable as Trav

import qualified AST

class ASTWalker a where
	walk :: Monad b => (
			AST.Program m -> b (AST.Program m),
			AST.Decl m -> b (AST.Decl m),
			AST.Type m -> b (AST.Type m),
			AST.Stmt m -> b (AST.Stmt m),
			AST.Expr m -> b (AST.Expr m),
			AST.Identifier m -> b (AST.Identifier m)
		) -> a m -> b (a m)

instance ASTWalker AST.Program where
	walk ftup@(fp, fd, ft, fs, fe, fi) p = do
		AST.Program decls m <- fp p
		decls <- Trav.mapM (walk ftup) decls
		return $ AST.Program decls m

instance ASTWalker AST.Decl where
	walk ftup@(_, fd, _, _, _, _) d = do
		d <- fd d
		case d of
			AST.VarDecl t i expr m -> do
				t <- walk ftup t
				i <- walk ftup i
				expr <- walk ftup expr
				return $ AST.VarDecl t i expr m
			AST.FunDecl t i args decls stmts attrs m -> do
				t <- walk ftup t
				i <- walk ftup i
				args <- Trav.mapM (\(x, y) -> do
					x <- walk ftup x
					y <- walk ftup y
					return (x, y)) args
				decls <- Trav.mapM (walk ftup) decls
				stmts <- Trav.mapM (walk ftup) stmts
				return $ AST.FunDecl t i args decls stmts attrs m
			AST.ExternDecl l t i args m -> do
				t <- walk ftup t
				i <- walk ftup i
				args <- Trav.mapM (\(x, y) -> do
					x <- walk ftup x
					y <- walk ftup y
					return (x, y)) args
				return $ AST.ExternDecl l t i args m

instance ASTWalker AST.Type where
	walk ftup@(_, _, ft, _, _, _) t = do
		t <- ft t
		case t of
			AST.TypeIdentifier i m -> do
				i <- walk ftup i
				return $ AST.TypeIdentifier i m
			AST.Product tx ty m -> do
				tx <- walk ftup tx
				ty <- walk ftup ty
				return $ AST.Product tx ty m
			AST.ListType tx m -> do
				tx <- walk ftup tx
				return $ AST.ListType tx m
			_ -> do
				return t

instance ASTWalker AST.Stmt where
	walk ftup@(_, _, _, fs, _, _) s = do
		s <- fs s
		case s of
			AST.Expr expr m -> do
				expr <- walk ftup expr
				return $ AST.Expr expr m
			AST.Scope stmts m -> do
				stmts <- Trav.mapM (walk ftup) stmts
				return $ AST.Scope stmts m
			AST.If expr stmtt m -> do
				expr <- walk ftup expr
				stmtt <- walk ftup stmtt
				return $ AST.If expr stmtt m
			AST.IfElse expr stmtt stmtf m -> do
				expr <- walk ftup expr
				stmtt <- walk ftup stmtt
				stmtf <- walk ftup stmtf
				return $ AST.IfElse expr stmtt stmtf m
			AST.While expr stmt m -> do
				expr <- walk ftup expr
				stmt <- walk ftup stmt
				return $ AST.While expr stmt m
			AST.Assignment i expr m -> do
				i <- walk ftup i
				expr <- walk ftup expr
				return $ AST.Assignment i expr m
			AST.Return Nothing m -> do
				return $ AST.Return Nothing m
			AST.Return (Just expr) m -> do
				expr <- walk ftup expr
				return $ AST.Return (Just expr) m

instance ASTWalker AST.Expr where
	walk ftup@(_, _, _, _, fe, _) e = do
		e <- fe e
		case e of
			AST.Var i m -> do
				i <- walk ftup i
				return $ AST.Var i m
			AST.Binop e1 op e2 m -> do
				e1 <- walk ftup e1
				e2 <- walk ftup e2
				return $ AST.Binop e1 op e2 m
			AST.Unop op expr m -> do
				expr <- walk ftup expr
				return $ AST.Unop op expr m
			AST.FunCall i exprs m -> do
				i <- walk ftup i
				exprs <- Trav.mapM (walk ftup) exprs
				return $ AST.FunCall i exprs m
			AST.Pair e1 e2 m -> do
				e1 <- walk ftup e1
				e2 <- walk ftup e2
				return $ AST.Pair e1 e2 m
			_ -> do
				return e

instance ASTWalker AST.Identifier where
	walk ftup@(_, _, _, _, _, fi) i = do
		i <- fi i
		return i
