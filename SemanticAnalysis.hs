module SemanticAnalysis (Context, Scope(..), Builtins(..), isBuiltin, GeneralIdentifier(..), P2, P2Meta(..), StringIdentifiable(..), bestMatch, ScopingError(..), ScopingWarning(..), ScopingResult, assignUniqueIDs, stripContext, typeOfBuiltin, AnnotatedType(..)) where

import Data.Maybe
import Text.EditDistance
import Data.List
import Data.Ord

import AST
import Meta
import Errors
import qualified Source
import Typing

-- b can be used for typing, not sure whether it will be useful
-- a will always be (AST.Identifier a)
type Context a b = [(a, b)]

-- Three levels of scoping (for warnings/errors)
data Scope = Global | Argument | Local
	deriving (Show, Eq, Read)

-- Builtin functions
-- TODO: Add more of them
data Builtins
	= Print
	| IsEmpty
	| Head
	| Tail
	| Fst
	| Snd
	deriving (Show, Eq, Read, Enum, Bounded)

isBuiltin :: Int -> Bool
isBuiltin n = n >= (fromEnum (minBound :: Builtins))
	&& n <= (fromEnum (maxBound :: Builtins))

-- Something in our context is either a builtin or a user defined thing
data GeneralIdentifier a = Builtin Builtins | User (AST.Identifier a)
	deriving (Show, Eq, Read)

-- Identifiers have scope and annotated type
type P1Context = Context (GeneralIdentifier P1Meta) (Scope, PolyType P1Meta)
data P2Meta = P2 {src2 :: Source.IndexSpan, context :: P1Context}
	deriving (Show, Eq, Read)
type P2 a = a P2Meta

-- TypeIndentifier to FTid
type TypeContext = [(String, FTid)]

-- Will be passed around and updated
type ScopingContext = (P1Context, TypeContext)

-- Forget the new structure (especially useful with fmap)
forget :: P2Meta -> P1Meta
forget thing = P1 {src = (src2 thing)}

-- To compare things in our context, we, in this phase, look at strings
class StringIdentifiable a where
	getString :: a -> String

stringEqual :: (StringIdentifiable a, StringIdentifiable b) => a -> b -> Bool
stringEqual a b = (getString a) == (getString b)

instance StringIdentifiable (AST.Identifier a) where
	getString = getIdentifierString

instance StringIdentifiable Builtins where
	getString Print		= "print"
	getString IsEmpty	= "isEmpty"
	getString Head		= "head"
	getString Tail		= "tail"
	getString Fst		= "fst"
	getString Snd		= "snd"

instance StringIdentifiable (GeneralIdentifier a) where
	getString (Builtin b) = getString b
	getString (User iden) = getString iden

idLookup :: StringIdentifiable a1 => a1 -> Context (GeneralIdentifier a2) b -> Maybe (GeneralIdentifier a2, b)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| stringEqual ident (fst x) = Just x
	| otherwise = idLookup ident xs

bestMatch :: StringIdentifiable a1 => a1 -> Context (GeneralIdentifier a2) b -> Maybe (GeneralIdentifier a2, b)
bestMatch search context = let (cost, best) = minimumBy (comparing fst) . map (\(ident, b) -> (restrictedDamerauLevenshteinDistance defaultEditCosts (getString search) (getString ident), (ident, b))) $ context in
	if cost<5 then Just best else Nothing

-- Updates a identifier to reflect a previous declared one (or builtin one)
updateIdentifier :: AST.Identifier a -> GeneralIdentifier b -> AST.Identifier a
updateIdentifier (AST.Identifier str _ a) (Builtin b) = AST.Identifier str (Just $ fromEnum b) a
updateIdentifier (AST.Identifier str _ a) (User (AST.Identifier _ m _)) = AST.Identifier str m a

maximalUniqueID :: Context (GeneralIdentifier a) b -> Maybe IdentID
maximalUniqueID [] = Nothing
maximalUniqueID ((Builtin b, _):xs) = case maximalUniqueID xs of
	Nothing -> Just $ fromEnum b
	Just m1 -> Just $ max (fromEnum b) m1
maximalUniqueID ((User (Identifier _ n _), _):xs) = case maximalUniqueID xs of
	Nothing -> n
	Just m1 -> case n of
		Nothing -> Just m1
		Just m2 -> Just $ max m1 m2

nextUniqueID :: Context (GeneralIdentifier a) b -> IdentID
nextUniqueID context = case maximalUniqueID context of
	Nothing -> 0
	Just n -> n+1

stripContext :: P1Context -> [IdentID]
stripContext [] = []
stripContext ((Builtin b,_):xs) = fromEnum b : stripContext xs
stripContext ((User (AST.Identifier _ n _),_):xs) = fromJust n : stripContext xs

-- First identifier is always the one in the source
data ScopingError = DuplicateDeclaration (P1 AST.Identifier) (P1 GeneralIdentifier) | UndeclaredIdentifier (P1 AST.Identifier) P1Context
data ScopingWarning = ShadowsDeclaration (P1 AST.Identifier) (P1 GeneralIdentifier) Scope
type ScopingResult b = ErrorContainer ScopingError ScopingWarning b

-- Empty context = all builtins
emptyContext :: P1Context
emptyContext = map (\x -> (Builtin x, (Global, fmap (const stupidP1Meta) (typeOfBuiltin x)))) [Print ..]
	where stupidP1Meta = constructP1 (Source.IndexSpan (-1) (-1))	-- TODO: fix this, if needed

startContext :: ScopingContext
startContext = (emptyContext, [])

-- Will rewrite AST such that all identifiers have an unique name (represented by an IdentID),
-- and add context (with scoping and annotated types) to the AST.
assignUniqueIDs :: P1 AST.Program -> ScopingResult (P2 AST.Program)
assignUniqueIDs program = do
	(program2, context) <- assignGlobs startContext program	-- 1) determine everything in global scope
	let program3 = (addContext context program2)		-- 2) add those things everywehere
	program4 <- assignAll program3				-- 3) then do the rest (functions/expressions, etc)
	return program4


-- Part One --
-- Returns a context with all top-level declarations
assignGlobs :: ScopingContext -> P1 AST.Program -> ScopingResult (P1 AST.Program, ScopingContext)
assignGlobs context (AST.Program decls m) = do
	(decls, context) <- assignGlobDecls context decls
	return (AST.Program decls m, context)

assignGlobDecls :: ScopingContext -> [P1 AST.Decl] -> ScopingResult ([P1 AST.Decl], ScopingContext)
assignGlobDecls context [] = return ([], context)
assignGlobDecls context (decl:xs) = do
	(decl, context) <- assignGlobDecl context decl
	(xs, context) <- assignGlobDecls context xs
	return (decl:xs, context)

assignGlobDecl :: ScopingContext -> P1 AST.Decl -> ScopingResult (P1 AST.Decl, ScopingContext)
assignGlobDecl context decl@(AST.VarDecl a ident b m) = do
	let ident2 = AST.assignUniqueID ident (nextUniqueID (fst context))
	let (at, newTypeContext) = annotatedType (snd context) decl
	case idLookup ident (fst context) of
		Just (iy, _) -> returnWithError (AST.VarDecl a ident b m, context) (DuplicateDeclaration ident iy)
		Nothing -> return (AST.VarDecl a ident2 b m, ((User ident2, (Global, at)):(fst context), newTypeContext))
assignGlobDecl context decl@(AST.FunDecl a ident b c d m) = do
	let ident2 = AST.assignUniqueID ident (nextUniqueID (fst context))
	let (at, newTypeContext) = annotatedType (snd context) decl
	case idLookup ident (fst context) of
		Just (iy, _) -> returnWithError (AST.FunDecl a ident b c d m, context) (DuplicateDeclaration ident iy)
		Nothing -> return (AST.FunDecl a ident2 b c d m, ((User ident2, (Global, at)):(fst context), newTypeContext))


-- Part Two --
addContextBasic :: ScopingContext -> P1Meta -> P2Meta
addContextBasic context meta = P2 {src2 = (src meta), context = (fst context)}

addContext :: ScopingContext -> P1 AST.Program -> P2 AST.Program
addContext context program = fmap (addContextBasic context) program

-- Used to assign context in subtrees (via fmap)
updateContext :: P1Context -> P2Meta -> P2Meta
updateContext newContext thing = thing { context = newContext }


-- Part Three --
assignAll :: P2 AST.Program -> ScopingResult (P2 AST.Program)
assignAll (AST.Program decls m) = do
	decls2 <- sequence (map assignDecl decls)
	return (AST.Program decls2 m)

assignDecl :: P2 AST.Decl -> ScopingResult (P2 AST.Decl)
assignDecl (AST.VarDecl a ident b m) = do
	y <- assignExpr b
	return (AST.VarDecl a ident y m)
assignDecl (AST.FunDecl t ident args decls stmtsin m) = do
	(args, context) <- assignFargs args (context m)
	(decls, context) <- assignVarDecls decls context
	let stmts = fmap (fmap (updateContext context)) stmtsin -- iterate over the list, iterate through tree
	stmts <- assignStmts stmts
	return (AST.FunDecl t ident args decls stmts m)

-- In this part we update the context, so we pass it to he function
assignFargs :: [(P2 AST.Type, P2 AST.Identifier)] -> P1Context -> ScopingResult ([(P2 AST.Type, P2 AST.Identifier)], P1Context)
assignFargs [] context = return ([], context)
assignFargs ((t,i):rest) context = do
	let x = (fmap (updateContext context) t, fmap (updateContext context) i)
	(x, context) <- assignFarg x context
	(rest, context) <- assignFargs rest context
	return ((x:rest), context)

assignFarg :: (P2 AST.Type, P2 AST.Identifier) -> P1Context -> ScopingResult ((P2 AST.Type, P2 AST.Identifier), P1Context)
assignFarg (t, ident) context = do
	let newIdent = AST.assignUniqueID ident (nextUniqueID context)
	let fident = (fmap forget newIdent)
	let (at, _) = annotatedType [] (fmap forget t)
	case idLookup ident context of
		Just (iy, (Global, _))	-> returnWithWarning ((t, newIdent), (User fident, (Argument, at)):context) (ShadowsDeclaration fident iy Global)
		Just (iy, _)		-> returnWithError ((t, newIdent), (User fident, (Argument, at)):context) (DuplicateDeclaration fident iy)
		Nothing			-> return ((t, newIdent), (User fident, (Argument, at)):context)

assignVarDecls :: [P2 AST.Decl] -> P1Context -> ScopingResult ([P2 AST.Decl], P1Context)
assignVarDecls [] context = return ([], context)
assignVarDecls (xin:rest) context = do
	let x = fmap (updateContext context) xin
	(x, context) <- assignVarDecl x context
	(rest, context) <- assignVarDecls rest context
	return ((x:rest), context)

assignVarDecl :: P2 AST.Decl -> P1Context -> ScopingResult (P2 AST.Decl, P1Context)
assignVarDecl decl@(AST.VarDecl a ident b m) context = do
	b <- assignExpr b
	let newIdent = AST.assignUniqueID ident (nextUniqueID context)
	let fident = (fmap forget newIdent)
	let (at, _) = annotatedType [] (fmap forget decl)
	case idLookup ident context of
		Just (iy, (Local, _))	-> returnWithError ((AST.VarDecl a newIdent b m), (User fident, (Local, at)):context) (DuplicateDeclaration fident iy)
		Just (iy, (scope, _))	-> returnWithWarning ((AST.VarDecl a newIdent b m), (User fident, (Local, at)):context) (ShadowsDeclaration fident iy scope)
		Nothing			-> return ((AST.VarDecl a newIdent b m), (User fident, (Local, at)):context)
assignVarDecl (AST.FunDecl _ _ _ _ _ _) _ = error "COMPILER BUG: Unexpected function declaration inside function body."

-- At this point, all contexts are fixed in the meta info
assignStmts :: [P2 AST.Stmt] -> ScopingResult ([P2 AST.Stmt])
assignStmts stmts = sequence (map assignStmt stmts)

assignStmt :: P2 AST.Stmt -> ScopingResult (P2 AST.Stmt)
assignStmt (Expr e m)		= do
	e <- assignExpr e
	return (Expr e m)
assignStmt (Scope stmts m)	= do
	stmts <- sequence (map assignStmt stmts)
	return (Scope stmts m)
assignStmt (If e stmt m)	= do
	e <- assignExpr e
	stmt <- assignStmt stmt
	return (If e stmt m)
assignStmt (IfElse e s1 s2 m)	= do
	e <- assignExpr e
	s1 <- assignStmt s1
	s2 <- assignStmt s2
	return (IfElse e s1 s2 m)
assignStmt (While e stmt m)	= do
	e <- assignExpr e
	stmt <- assignStmt stmt
	return (While e stmt m)
assignStmt (Assignment ident e m)	= do
	e <- assignExpr e
	case idLookup ident (context m) of
		Just (iy, _) -> return (Assignment (updateIdentifier ident iy) e m)
		Nothing -> returnWithError (Assignment ident e m) (UndeclaredIdentifier (fmap forget ident) (context m))
assignStmt (Return (Just e) m)	= do
	e <- assignExpr e
	return (Return (Just e) m)
assignStmt x = return x -- (Return Nothing m)

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
assignExpr (AST.Pair e1 e2 m) = do
	ee1 <- assignExpr e1
	ee2 <- assignExpr e2
	return $ AST.Pair ee1 ee2 m
assignExpr x = return x -- ignores constants


-- Annotation of types --
typeOfBuiltin :: Builtins -> PolyType ()
typeOfBuiltin x = typeOfBuiltin2 x ()
	where
	typeOfBuiltin2 Print	= Poly (FT 0 ()) $ Mono (Func [Free (FT 0 ()) ()] (Typing.Void ()) ()) ()
	typeOfBuiltin2 IsEmpty	= Poly (FT 0 ()) $ Mono (Func [List (Free (FT 0 ()) ()) ()] (Typing.Bool ()) ()) ()
	typeOfBuiltin2 Head	= Poly (FT 0 ()) $ Mono (Func [List (Free (FT 0 ()) ()) ()] (Free (FT 0 ()) ()) ()) ()
	typeOfBuiltin2 Tail	= Poly (FT 0 ()) $ Mono (Func [List (Free (FT 0 ()) ()) ()] (List (Free (FT 0 ()) ()) ()) ()) ()
	typeOfBuiltin2 Fst	= Poly (FT 0 ()) $ Poly (FT 1 ()) (Mono (Func [Typing.Pair (Free (FT 0 ()) ()) (Free (FT 1 ()) ()) ()] (Free (FT 0 ()) ()) ()) () ) ()
	typeOfBuiltin2 Snd	= Poly (FT 0 ()) $ Poly (FT 1 ()) (Mono (Func [Typing.Pair (Free (FT 0 ()) ()) (Free (FT 1 ()) ()) ()] (Free (FT 1 ()) ()) ()) () ) ()

-- return all identifiers occuring in type
typeIdentifiers :: AST.Type a -> [String] -> [String]
typeIdentifiers (AST.Void _) l		= l
typeIdentifiers (AST.Int _) l		= l
typeIdentifiers (AST.Bool _) l		= l
typeIdentifiers (TypeIdentifier ident _) l = l `union` [getString ident]
typeIdentifiers (Product a b _) l	= (typeIdentifiers a l) `union` (typeIdentifiers b l)
typeIdentifiers (ListType a _) l	= typeIdentifiers a l

annotatedMType :: TypeContext -> AST.Type a -> MonoType a
annotatedMType pc (AST.Void m)		= Typing.Void m
annotatedMType pc (AST.Int m)		= Typing.Int m
annotatedMType pc (AST.Bool m)		= Typing.Bool m
annotatedMType pc (TypeIdentifier ident m) = Free (FT (fromJust (lookup (getString ident) pc)) m) m
annotatedMType pc (Product a b m)	= Typing.Pair (annotatedMType pc a) (annotatedMType pc b) m
annotatedMType pc (ListType a m)	= List (annotatedMType pc a) m

-- Wrap the mono in a poly, with given context
poly :: TypeContext -> MonoType a -> PolyType a
poly [] mt = Mono mt (getMeta mt)
poly ((_,n):xs) mt = Poly (FT n m) (poly xs mt) m
	where m = getMeta mt

class AnnotatedType b where
	annotatedType :: TypeContext -> b a  -> (PolyType a, TypeContext)

-- We can do variables and functions
instance AnnotatedType AST.Decl where
	annotatedType c (VarDecl t ident b m) = annotatedType c t
	annotatedType c (FunDecl rt ident args _ _ m) = (poly pc (Func argTypes (annotatedMType pc rt) m), c)
		where
			allIdents = nub $ concat $ map (\t -> typeIdentifiers t []) (rt : map fst args)
			pc = zip allIdents [0..]
			argTypes = map (annotatedMType pc . fst) args

-- And just types (eg. in VarDecl or arguments of function)
instance AnnotatedType Type where
	annotatedType c t = (Mono (annotatedMType pc t) (getMeta t), pc)
		where
			nextid = case c of
				[] -> 0
				cs -> maximum (map snd cs) + 1
			newTIs = (typeIdentifiers t []) \\ (map fst c)
			pc = c ++ zip newTIs [nextid..]
