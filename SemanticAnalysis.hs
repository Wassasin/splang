module SemanticAnalysis (Context, P2, ScopingError(..), ScopingResult(..), assignUniqueIDs) where

import AST
import Meta
import qualified Source

-- b can be used for typing, not sure whether it will be useful
type Context a b = [(AST.Identifier a, b)]

data P2Meta = P2 {src2 :: Source.IndexSpan, context :: Context P1Meta ()}
	deriving (Show, Eq, Read)

type P2 a = a P2Meta

forget :: P2Meta -> P1Meta
forget thing = P1 {src = (src2 thing)}

idLookup :: AST.Identifier c -> Context a b -> Maybe (AST.Identifier a, b)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| AST.getIdentifierString ident == AST.getIdentifierString (fst x) = Just x
	| otherwise = idLookup ident xs

updateIdentifier :: AST.Identifier a -> AST.Identifier b -> AST.Identifier a
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

data ScopingError a = DuplicateDeclaration (AST.Identifier a) (AST.Identifier a)
	| UndeclaredIdentifier (AST.Identifier a) (Context a ())
data ScopingResult a b = Result b [ScopingError a] | FatalError [ScopingError a]

instance Monad (ScopingResult a) where
	return x = Result x []
	(>>=) (Result y errors) f = case f y of
		Result z errors2 -> Result z (errors ++ errors2)
		FatalError errors2 -> FatalError (errors ++ errors2)
	(>>=) (FatalError e) f = FatalError e

-- Will rewrite AST such that all identifiers have an unique name (represented by an Int)
assignUniqueIDs :: P1 AST.Program -> ScopingResult P1Meta (P2 AST.Program)
assignUniqueIDs program = do
	(program2, context) <- assignGlobs program		-- 1) determine everything in global scope
	let program3 = (addContext context program2)		-- 2) add those things everywehere (TODO: also add builtins)
	program4 <- assignAll program3				-- 3) then do the rest (functions/expressions, etc)
	return program4

-- Part One --
-- Returns a context with all top-level declarations
assignGlobs :: P1 AST.Program -> ScopingResult P1Meta (P1 AST.Program, Context P1Meta ())
assignGlobs (AST.Program decls m) = do
	(decls2, fcontext) <- assignGlobDecls [] decls
	return (AST.Program decls2 m, fcontext)

assignGlobDecls :: Context P1Meta () -> [P1 AST.Decl] -> ScopingResult P1Meta ([P1 AST.Decl], Context P1Meta ())
assignGlobDecls context [] = return ([], context)
assignGlobDecls context (decl:xs) = do
	(decl2, context2) <- assignGlobDecl context decl
	(ys, fcontext) <- assignGlobDecls context2 xs
	return (decl2:ys, fcontext)

assignGlobDecl :: Context P1Meta () -> P1 AST.Decl -> ScopingResult P1Meta (P1 AST.Decl, Context P1Meta ())
assignGlobDecl context (AST.VarDecl a ident b m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.VarDecl a ident b m, context) [DuplicateDeclaration ident iy]
	Nothing -> return (AST.VarDecl a ident2 b m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)
assignGlobDecl context (AST.FunDecl a ident b c d m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.FunDecl a ident b c d m, context) [DuplicateDeclaration ident iy]
	Nothing -> return (AST.FunDecl a ident2 b c d m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)

-- Part Two --
addContextBasic :: Context P1Meta () -> P1Meta -> P2Meta
addContextBasic context meta = P2 {src2 = (src meta), context = context}

addContext :: Context P1Meta () -> AST.Program P1Meta -> AST.Program P2Meta
addContext context program = fmap (addContextBasic context) program

-- Part Three --
-- This will also go inside functions and check whether every identifier is declared
assignAll :: P2 AST.Program -> ScopingResult P1Meta (P2 AST.Program)
assignAll (AST.Program decls m) = do
	decls2 <- sequence (map assignDecl decls)
	return (AST.Program decls2 m)

assignDecl :: P2 AST.Decl -> ScopingResult P1Meta (P2 AST.Decl)
assignDecl (AST.VarDecl a ident b m) = do
	y <- assignExpr b
	return (AST.VarDecl a ident y m)
assignDecl x = return x

assignExpr :: P2 AST.Expr -> ScopingResult P1Meta (P2 AST.Expr)
assignExpr (Var ident m)  = case idLookup ident (context m) of
		Just (iy, _) -> return (Var (updateIdentifier ident iy) m)
		Nothing -> Result (Var ident m) [UndeclaredIdentifier (fmap forget ident) (context m)]
assignExpr (Binop e1 bop e2 m) = do
	ee1 <- assignExpr e1
	ee2 <- assignExpr e2
	return $ Binop ee1 bop ee2 m
assignExpr (Unop uop e m) = do
	e2 <- assignExpr e
	return $ Unop uop e2 m
assignExpr (FunCall ident exprs m) = case idLookup ident (context m) of
	Just (iy, _) -> do
		exprs2 <- sequence (map assignExpr exprs)
		return $ FunCall (updateIdentifier ident iy) exprs2 m
	Nothing -> Result (FunCall ident exprs m) [UndeclaredIdentifier (fmap forget ident) (context m)]
assignExpr (Pair e1 e2 m) = do
	ee1 <- assignExpr e1
	ee2 <- assignExpr e2
	return $ Pair ee1 ee2 m
assignExpr (List exprs m) = do
	exprs2 <- sequence (map assignExpr exprs)
	return $ List exprs2 m
assignExpr x = return x -- ignores constants
