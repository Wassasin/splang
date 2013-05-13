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
	walk ftup@(fp, fd, ft, fs, fe, fi) p@(AST.Program decls _) = do
		AST.Program decls m <- fp p
		decls <- Trav.mapM (walk ftup) decls
		return $ AST.Program decls m

instance ASTWalker AST.Decl where
	walk ftup@(fp, fd, ft, fs, fe, fi) d@(AST.VarDecl t i expr _) = do
		AST.VarDecl t i expr m <- fd d
		t <- walk ftup t
		i <- walk ftup i
		expr <- walk ftup expr
		return $ AST.VarDecl t i expr m
	walk ftup@(fp, fd, ft, fs, fe, fi) d@(AST.FunDecl t i args decls stmts _) = do
		AST.FunDecl t i args decls stmts m <- fd d
		t <- walk ftup t
		i <- walk ftup i
		args <- Trav.mapM (\(x, y) -> do
			x <- walk ftup x
			y <- walk ftup y
			return (x, y)) args
		decls <- Trav.mapM (walk ftup) decls
		stmts <- Trav.mapM (walk ftup) stmts
		return $ AST.FunDecl t i args decls stmts m

instance ASTWalker AST.Type where
	walk ftup@(fp, fd, ft, fs, fe, fi) t@(AST.TypeIdentifier i _) = do
		AST.TypeIdentifier i m <- ft t
		i <- walk ftup i
		return $ AST.TypeIdentifier i m
	walk ftup@(fp, fd, ft, fs, fe, fi) t@(AST.Product tx ty _) = do
		AST.Product tx ty m <- ft t
		tx <- walk ftup tx
		ty <- walk ftup ty
		return $ AST.Product tx ty m
	walk ftup@(fp, fd, ft, fs, fe, fi) t@(AST.ListType tx _) = do
		AST.ListType tx m <- ft t
		tx <- walk ftup tx
		return $ AST.ListType tx m
	walk ftup@(fp, fd, ft, fs, fe, fi) t = do
		t <- ft t
		return t

instance ASTWalker AST.Stmt where
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.Scope stmts _) = do
		AST.Scope stmts m <- fs s
		stmts <- Trav.mapM (walk ftup) stmts
		return $ AST.Scope stmts m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.If expr stmtt _) = do
		AST.If expr stmtt m <- fs s
		expr <- walk ftup expr
		stmtt <- walk ftup stmtt
		return $ AST.If expr stmtt m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.IfElse expr stmtt stmtf _) = do
		AST.IfElse expr stmtt stmtf m <- fs s
		expr <- walk ftup expr
		stmtt <- walk ftup stmtt
		stmtf <- walk ftup stmtf
		return $ AST.IfElse expr stmtt stmtf m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.While expr stmt _) = do
		AST.While expr stmt m <- fs s
		expr <- walk ftup expr
		stmt <- walk ftup stmt
		return $ AST.While expr stmt m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.Assignment i expr _) = do
		AST.Assignment i expr m <- fs s
		i <- walk ftup i
		expr <- walk ftup expr
		return $ AST.Assignment i expr m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.Return Nothing _) = do
		AST.Return Nothing m <- fs s
		return $ AST.Return Nothing m
	walk ftup@(fp, fd, ft, fs, fe, fi) s@(AST.Return (Just expr) _) = do
		AST.Return (Just expr) m <- fs s
		expr <- walk ftup expr
		return $ AST.Return (Just expr) m

instance ASTWalker AST.Expr where
	walk ftup@(fp, fd, ft, fs, fe, fi) e@(AST.Var _ _) = do
		AST.Var i m <- fe e
		i <- walk ftup i
		return $ AST.Var i m
	walk ftup@(fp, fd, ft, fs, fe, fi) e@(AST.Binop _ _ _ _) = do
		AST.Binop e1 op e2 m <- fe e
		e1 <- walk ftup e1
		e2 <- walk ftup e2
		return $ AST.Binop e1 op e2 m
	walk ftup@(fp, fd, ft, fs, fe, fi) e@(AST.Unop _ _ _) = do
		AST.Unop op expr m <- fe e
		expr <- walk ftup expr
		return $ AST.Unop op expr m
	walk ftup@(fp, fd, ft, fs, fe, fi) e@(AST.FunCall _ _ _) = do
		AST.FunCall i exprs m <- fe e
		i <- walk ftup i
		exprs <- Trav.mapM (walk ftup) exprs
		return $ AST.FunCall i exprs m
	walk ftup@(fp, fd, ft, fs, fe, fi) e@(AST.Pair _ _ _) = do
		AST.Pair e1 e2 m <- fe e
		e1 <- walk ftup e1
		e2 <- walk ftup e2
		return $ AST.Pair e1 e2 m
	walk ftup@(fp, fd, ft, fs, fe, fi) e = do
		e <- fe e
		return e

instance ASTWalker AST.Identifier where
	walk ftup@(fp, fd, ft, fs, fe, fi) i = do
		i <- fi i
		return i

class ASTWalkerNop a where
	nop :: Monad b => a m -> b (a m)
	nop x = return x
