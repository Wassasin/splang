module SemanticAnalysis (Context, ScopingError(..), ScopingResult(..), assignUniqueIDs) where

import AST

type Context a b = [(AST.Identifier a, b)]

idLookup :: AST.Identifier a -> Context a b -> Maybe (AST.Identifier a, b)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| AST.getIdentifierString ident == AST.getIdentifierString (fst x) = Just x
	| otherwise = idLookup ident xs

updateIdentifier :: AST.Identifier a -> AST.Identifier a -> AST.Identifier a
updateIdentifier (AST.Identifier str n a) (AST.Identifier str2 m b) = AST.Identifier str m a

maximalUniqueID :: Context a b -> Maybe Int
maximalUniqueID [] = Nothing
maximalUniqueID ((Identifier _ n _, _):xs) = case maximalUniqueID xs of
	Nothing -> n
	Just m1 -> case n of
		Nothing -> Just m1
		Just m2 -> Just $ max m1 m2

nextUniqueID :: Context a b -> Int
nextUniqueID context = case maximalUniqueID context of
	Nothing -> 0
	Just n -> n+1

data ScopingError a c = DuplicateDeclaration (AST.Identifier a) (AST.Identifier a)
	| UndeclaredIdentifier (AST.Identifier a) (Context a c)
data ScopingResult a b = Result b [ScopingError a ()] | FatalError [ScopingError a ()]

instance Monad (ScopingResult a) where
	return x = Result x []
	(>>=) (Result y errors) f = case f y of
		Result z errors2 -> Result z (errors ++ errors2)
		FatalError errors2 -> FatalError (errors ++ errors2)
	(>>=) (FatalError e) f = FatalError e

-- Will rewrite AST such that all identifiers have an unique name (represented by an Int)
assignUniqueIDs :: AST.Program a -> ScopingResult a (AST.Program a)
assignUniqueIDs program = do
	(program2, context) <- assignGlobs program
	program3 <- assignAll context program2
	return program3

-- Returns a context with all top-level declarations
assignGlobs :: AST.Program a -> ScopingResult a (AST.Program a, Context a ())
assignGlobs (AST.Program decls m) = do
	(decls2, fcontext) <- assignGlobDecls [] decls
	return (AST.Program decls2 m, fcontext)

assignGlobDecls :: Context a () -> [AST.Decl a] -> ScopingResult a ([AST.Decl a], Context a ())
assignGlobDecls context [] = return ([], context)
assignGlobDecls context (decl:xs) = do
	(decl2, context2) <- assignGlobDecl context decl
	(ys, fcontext) <- assignGlobDecls context2 xs
	return (decl2:ys, fcontext)

assignGlobDecl :: Context a () -> AST.Decl a -> ScopingResult a (AST.Decl a, Context a ())
assignGlobDecl context (AST.VarDecl a ident b m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.VarDecl a ident b m, context) [DuplicateDeclaration ident iy]
	Nothing -> return (AST.VarDecl a ident2 b m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)
assignGlobDecl context (AST.FunDecl a ident b c d m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.FunDecl a ident b c d m, context) [DuplicateDeclaration ident iy]
	Nothing -> return (AST.FunDecl a ident2 b c d m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)

-- This will also go inside functions and check whether every identifier is declared
assignAll :: Context a () -> AST.Program a -> ScopingResult a (AST.Program a)
assignAll context (AST.Program decls m) = do
	decls2 <- sequence (map (assignDecl context) decls)
	return (AST.Program decls2 m)

assignDecl :: Context a () -> AST.Decl a -> ScopingResult a (AST.Decl a)
assignDecl context (AST.VarDecl a ident b m) = do
	y <- assignExpr context b
	return (AST.VarDecl a ident y m)
assignDecl context x = return x

assignExpr :: Context a () -> AST.Expr a -> ScopingResult a (AST.Expr a)
assignExpr context (Var ident m)  = case idLookup ident context of
		Just (iy, _) -> return (Var (updateIdentifier ident iy) m)
		Nothing -> Result (Var ident m) [UndeclaredIdentifier ident context]
assignExpr context (Binop e1 bop e2 m) = do
	ee1 <- assignExpr context e1
	ee2 <- assignExpr context e2
	return $ Binop ee1 bop ee2 m
assignExpr context (Unop uop e m) = do
	e2 <- assignExpr context e
	return $ Unop uop e2 m
assignExpr context (FunCall ident exprs m) = case idLookup ident context of
	Just (iy, _) -> do
		exprs2 <- sequence (map (assignExpr context) exprs)
		return $ FunCall (updateIdentifier ident iy) exprs2 m
	Nothing -> Result (FunCall ident exprs m) [UndeclaredIdentifier ident context]
assignExpr context (Pair e1 e2 m) = do
	ee1 <- assignExpr context e1
	ee2 <- assignExpr context e2
	return $ Pair ee1 ee2 m
assignExpr context (List exprs m) = do
	exprs2 <- sequence (map (assignExpr context) exprs)
	return $ List exprs2 m
assignExpr context x = return x
