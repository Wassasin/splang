module SemanticAnalysis (Context, P2, ScopingError(..), ScopingWarning(..), ScopingResult(..), assignUniqueIDs) where

import AST
import Meta
import Errors
import qualified Source

-- b can be used for typing, not sure whether it will be useful
-- a will always be (AST.Identifier a)
type Context a b = [(a, b)]

type LocatedIdentifier = P1 AST.Identifier
type P1Context = Context LocatedIdentifier ()

data P2Meta = P2 {src2 :: Source.IndexSpan, context :: P1Context}
	deriving (Show, Eq, Read)

type P2 a = a P2Meta

forget :: P2Meta -> P1Meta
forget thing = P1 {src = (src2 thing)}

idLookup :: AST.Identifier c -> Context (AST.Identifier a) b -> Maybe (AST.Identifier a, b)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| AST.getIdentifierString ident == AST.getIdentifierString (fst x) = Just x
	| otherwise = idLookup ident xs

updateIdentifier :: AST.Identifier a -> AST.Identifier b -> AST.Identifier a
updateIdentifier (AST.Identifier str n a) (AST.Identifier str2 m b) = AST.Identifier str m a

maximalUniqueID :: Context (AST.Identifier a) b -> Maybe Int
maximalUniqueID [] = Nothing
maximalUniqueID ((Identifier _ n _, _):xs) = case maximalUniqueID xs of
	Nothing -> n
	Just m1 -> case n of
		Nothing -> Just m1
		Just m2 -> Just $ max m1 m2

nextUniqueID :: Context (AST.Identifier a) b -> Int
nextUniqueID context = case maximalUniqueID context of
	Nothing -> 0
	Just n -> n+1

-- a will most probably be some kind of identifier
data ScopingError = DuplicateDeclaration (P1 AST.Identifier) (P1 AST.Identifier) | UndeclaredIdentifier (P1 AST.Identifier) (Context (P1 AST.Identifier) ())
data ScopingWarning = ShadowsDeclaration (P1 AST.Identifier) (P1 AST.Identifier)
-- a is used for error messages
type ScopingResult b = ErrorContainer ScopingError ScopingWarning b

-- Will rewrite AST such that all identifiers have an unique name (represented by an Int)
assignUniqueIDs :: P1 AST.Program -> ScopingResult (P2 AST.Program)
assignUniqueIDs program = do
	(program2, context) <- assignGlobs program		-- 1) determine everything in global scope
	let program3 = (addContext context program2)		-- 2) add those things everywehere
	program4 <- assignAll program3				-- 3) then do the rest (functions/expressions, etc)
	return program4

-- Part One --
-- Returns a context with all top-level declarations
assignGlobs :: P1 AST.Program -> ScopingResult (P1 AST.Program, P1Context)
assignGlobs (AST.Program decls m) = do
	(decls2, fcontext) <- assignGlobDecls [] decls
	return (AST.Program decls2 m, fcontext)

assignGlobDecls :: P1Context -> [P1 AST.Decl] -> ScopingResult ([P1 AST.Decl], P1Context)
assignGlobDecls context [] = return ([], context)
assignGlobDecls context (decl:xs) = do
	(decl2, context2) <- assignGlobDecl context decl
	(ys, fcontext) <- assignGlobDecls context2 xs
	return (decl2:ys, fcontext)

assignGlobDecl :: P1Context -> P1 AST.Decl -> ScopingResult (P1 AST.Decl, P1Context)
assignGlobDecl context (AST.VarDecl a ident b m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.VarDecl a ident b m, context) [DuplicateDeclaration ident iy] []
	Nothing -> return (AST.VarDecl a ident2 b m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)
assignGlobDecl context (AST.FunDecl a ident b c d m) = case idLookup ident context of
	Just (iy, _) -> Result (AST.FunDecl a ident b c d m, context) [DuplicateDeclaration ident iy] []
	Nothing -> return (AST.FunDecl a ident2 b c d m, (ident2,()):context)
	where ident2 = AST.assignUniqueID ident (nextUniqueID context)

-- Part Two --
-- TODO: also add builtins
addContextBasic :: P1Context -> P1Meta -> P2Meta
addContextBasic context meta = P2 {src2 = (src meta), context = context}

addContext :: P1Context -> P1 AST.Program -> P2 AST.Program
addContext context program = fmap (addContextBasic context) program

-- Part Three --
-- TODO: go into functions
assignAll :: P2 AST.Program -> ScopingResult (P2 AST.Program)
assignAll (AST.Program decls m) = do
	decls2 <- sequence (map assignDecl decls)
	return (AST.Program decls2 m)

assignDecl :: P2 AST.Decl -> ScopingResult (P2 AST.Decl)
assignDecl (AST.VarDecl a ident b m) = do
	y <- assignExpr b
	return (AST.VarDecl a ident y m)
assignDecl (AST.FunDecl t ident args decls stmts m) = do
	(args, context) <- assignFargs args (context m)
	(decls, context) <- assignVarDecls decls context
	--stmts <- assignStmts stmts context
	return (AST.FunDecl t ident args decls stmts m)

assignFargs :: [(P2 AST.Type, P2 AST.Identifier)] -> P1Context -> ScopingResult ([(P2 AST.Type, P2 AST.Identifier)], P1Context)
assignFargs [] context = return ([], context)
assignFargs (x:rest) context = do
	(x, context) <- assignFarg x context
	(rest, context) <- assignFargs rest context
	return ((x:rest), context)

-- TODO: update UID and context in meta, and perhaps remove shadowed decl from context?
assignFarg :: (P2 AST.Type, P2 AST.Identifier) -> P1Context -> ScopingResult ((P2 AST.Type, P2 AST.Identifier), P1Context)
assignFarg (t, ident) context = do
	let newIdent = AST.assignUniqueID ident (nextUniqueID context)
	let fident = (fmap forget newIdent)
	case idLookup ident context of
		Just (iy, _) -> returnWithWarning ((t, newIdent), (fident,()):context) (ShadowsDeclaration fident iy)
		Nothing -> return ((t, newIdent), (fident,()):context)

assignVarDecls :: [P2 AST.Decl] -> P1Context -> ScopingResult ([P2 AST.Decl], P1Context)
assignVarDecls [] context = return ([], context)
assignVarDecls (x:rest) context = do
	(x, context) <- assignVarDecl x context
	(rest, context) <- assignVarDecls rest context
	return ((x:rest), context)

-- TODO: update UID and context in meta, and perhaps remove shadowed decl from context?
assignVarDecl :: P2 AST.Decl -> P1Context -> ScopingResult (P2 AST.Decl, P1Context)
assignVarDecl (AST.VarDecl a ident b m) context = do
	let newIdent = AST.assignUniqueID ident (nextUniqueID context)
	let fident = (fmap forget newIdent)
	case idLookup ident context of
		Just (iy, _) -> returnWithWarning ((AST.VarDecl a newIdent b m), (fident,()):context) (ShadowsDeclaration fident iy)
		Nothing -> return ((AST.VarDecl a newIdent b m), (fident,()):context)


assignExpr :: P2 AST.Expr -> ScopingResult (P2 AST.Expr)
assignExpr (Var ident m)  = case idLookup ident (context m) of
		Just (iy, _) -> return (Var (updateIdentifier ident iy) m)
		Nothing -> returnWithError (Var ident m) (UndeclaredIdentifier (fmap forget ident) (context m))
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
	Nothing -> returnWithError (FunCall ident exprs m) (UndeclaredIdentifier (fmap forget ident) (context m))
assignExpr (Pair e1 e2 m) = do
	ee1 <- assignExpr e1
	ee2 <- assignExpr e2
	return $ Pair ee1 ee2 m
assignExpr (List exprs m) = do
	exprs2 <- sequence (map assignExpr exprs)
	return $ List exprs2 m
assignExpr x = return x -- ignores constants
