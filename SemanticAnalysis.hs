module SemanticAnalysis (Context, Scope(..), Builtins(..), isBuiltin, builtinP2Meta, GeneralIdentifier(..), P1Context, P2, P2Meta(context, annotatedType), StringIdentifiable(..), bestMatch, ScopingError(..), ScopingWarning(..), ScopingResult, assignUniqueIDs, stripContext, typeOfBuiltin) where

import Data.Maybe
import Text.EditDistance
import Data.List
import Data.Ord

import AST
import Meta
import Errors
import qualified Source
import Typing
import Builtins

-- b can be used for typing, not sure whether it will be useful
-- a will always be (AST.Identifier a)
type Context a b = [(a, b)]

-- Three levels of scoping (for warnings/errors)
data Scope = Global | Argument | Local
	deriving (Show, Eq, Read)

-- Something in our context is either a builtin or a user defined thing
data GeneralIdentifier a = Builtin Builtins | User (AST.Identifier a)
	deriving (Show, Eq, Read)

-- TypeIndentifier to FTid
type TypeContext = [(String, FTid)]

-- Identifiers have scope and annotated type and TypeContext for arguments (only in fundecls)
type P1Context = Context (GeneralIdentifier P1Meta) (Scope, PolyType P2Meta)
data P2Meta = P2 {source2 :: Source.IndexSpan, context :: P1Context, annotatedType :: Maybe (PolyType P2Meta), argsTypeContext :: TypeContext, functionLinkageContext :: Context (GeneralIdentifier P2Meta) (AST.ExternLanguage P2Meta)}
	deriving (Show, Eq, Read)
type P2 a = a P2Meta

instance Source.Sourcable P2Meta where
	src = source2

-- Will be passed around and updated
data ScopingContext = ScopingContext {identifiers :: P1Context, types :: TypeContext, nextFTid :: Int, nextUniqueID :: Int, functionLinkage :: Context (GeneralIdentifier P2Meta) (AST.ExternLanguage P2Meta)}

addIdentifier :: P1 AST.Identifier -> Scope -> PolyType P2Meta -> ScopingContext -> ScopingContext
addIdentifier ident s at context = context { identifiers = (User ident, (s, at)) : identifiers context, nextUniqueID = next }
	where
		(AST.Identifier _ n _ _) = ident
		next = case n of
			Nothing -> nextUniqueID context
			Just m -> max (m+1) (nextUniqueID context)

-- Forget the new structure (especially useful with fmap)
forget :: P2Meta -> P1Meta
forget thing = constructP1 (source2 thing)

-- Contruct empte P2Meta from P1Meta
promote :: P1Meta -> P2Meta
promote m = P2 { source2 = Source.src m, context = [], annotatedType = Nothing, argsTypeContext = [], functionLinkageContext = [] }

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
updateIdentifier (AST.Identifier str _ l a) (Builtin b) = AST.Identifier str (Just $ fromEnum b) l a
updateIdentifier (AST.Identifier str _ l a) (User (AST.Identifier _ m _ _)) = AST.Identifier str m l a

stripContext :: P1Context -> [IdentID]
stripContext [] = []
stripContext ((Builtin b,_):xs) = fromEnum b : stripContext xs
stripContext ((User (AST.Identifier _ n _ _),_):xs) = fromJust n : stripContext xs

annotateType :: PolyType P2Meta -> P2Meta -> P2Meta
annotateType t m = m { annotatedType = Just t }


-- First identifier is always the one in the source
data ScopingError = DuplicateDeclaration (P1 AST.Identifier) (P1 GeneralIdentifier) | UndeclaredIdentifier (P1 AST.Identifier) P1Context
data ScopingWarning = ShadowsDeclaration (P1 AST.Identifier) (P1 GeneralIdentifier) Scope
type ScopingResult b = ErrorContainer ScopingError ScopingWarning b

-- Empty context = all builtins
emptyContext :: P1Context
emptyContext = map (\x -> (Builtin x, (Global, fmap (const builtinP2Meta) (typeOfBuiltin x)))) [Print ..]

builtinP2Meta :: P2Meta
builtinP2Meta = promote $ constructP1 (Source.IndexSpan (-1) (-1))	-- TODO: fix this, if needed

startContext :: ScopingContext
startContext = ScopingContext { identifiers = emptyContext, types = [], nextFTid = 0, nextUniqueID = fromEnum (maxBound :: Builtins) + 1, functionLinkage = [] }

-- Will rewrite AST such that all identifiers have an unique name (represented by an IdentID),
-- and add context (with scoping and annotated types) to the AST.
assignUniqueIDs :: P1 AST.Program -> ScopingResult (P2 AST.Program)
assignUniqueIDs program = do
	let program1 = fmap promote program
	(program2, context) <- assignGlobs startContext program1-- 1) determine everything in global scope
	let program3 = fmap (updateContext context) program2	-- 2) add those things everywehere
	program4 <- assignAll context program3			-- 3) then do the rest (functions/expressions, etc)
	return program4


-- Part One --
-- Returns a context with all top-level declarations
assignGlobs :: ScopingContext -> P2 AST.Program -> ScopingResult (P2 AST.Program, ScopingContext)
assignGlobs context (AST.Program decls m) = do
	(decls, context) <- assignGlobDecls context decls
	return (AST.Program decls m, context)

assignGlobDecls :: ScopingContext -> [P2 AST.Decl] -> ScopingResult ([P2 AST.Decl], ScopingContext)
assignGlobDecls context [] = return ([], context)
assignGlobDecls context (decl:xs) = do
	(decl, context) <- assignGlobDecl context decl
	(xs, context) <- assignGlobDecls context xs
	return (decl:xs, context)

assignGlobDecl :: ScopingContext -> P2 AST.Decl -> ScopingResult (P2 AST.Decl, ScopingContext)
assignGlobDecl context decl@(AST.VarDecl a ident b m) = do
	let ident2 = AST.assignUniqueID ident (nextUniqueID context)
	let (at, newContext) = getAnnotatedType context decl
	case idLookup ident (identifiers context) of
		Just (iy, _) -> returnWithError (AST.VarDecl a ident b m, newContext) (DuplicateDeclaration (fmap forget ident) iy)
		Nothing -> return (AST.VarDecl a ident2 b m, addIdentifier (fmap forget ident2) Global at newContext)
assignGlobDecl context decl@(AST.FunDecl a ident b c d attrs m) = do
	let el = ExternLanguage "SPL" m
	let ident2 = setIdentInfo (IdentInfo {externLanguage=el}) $ AST.assignUniqueID ident (nextUniqueID context)
	let (at, argTypeContext) = getAnnotatedType context decl
	let m2 = m { argsTypeContext = types argTypeContext }
	let newContext = context { nextFTid = nextFTid argTypeContext, functionLinkage = (User ident, el):functionLinkage argTypeContext }
	let newAttrs = case getIdentifierString ident of
		"main" -> union [AST.Export] attrs
		_ -> attrs
	case idLookup ident (identifiers context) of
		Just (iy, _) -> returnWithError (AST.FunDecl a ident b c d newAttrs m2, newContext) (DuplicateDeclaration (fmap forget ident) iy)
		Nothing -> return (AST.FunDecl a ident2 b c d newAttrs m2, addIdentifier (fmap forget ident2) Global at newContext)
assignGlobDecl context decl@(AST.ExternDecl l a ident b m) = do
	let el = l
	let ident2 = setIdentInfo (IdentInfo {externLanguage=el}) $ AST.assignUniqueID ident (nextUniqueID context)
	let (at, argTypeContext) = getAnnotatedType context decl
	let m2 = m { argsTypeContext = types argTypeContext }
	let newContext = context { nextFTid = nextFTid argTypeContext, functionLinkage = (User ident, el):functionLinkage argTypeContext }
	case idLookup ident (identifiers context) of
		Just (iy, _) -> returnWithError (AST.ExternDecl l a ident b m2, newContext) (DuplicateDeclaration (fmap forget ident) iy)
		Nothing -> return (AST.ExternDecl l a ident2 b m2, addIdentifier (fmap forget ident2) Global at newContext)

-- Part Two --
-- Used to assign context in subtrees (via fmap)
updateContext :: ScopingContext -> P2Meta -> P2Meta
updateContext newContext thing = thing { context = (identifiers newContext), functionLinkageContext = (functionLinkage newContext) }


-- Part Three --
assignAll :: ScopingContext -> P2 AST.Program -> ScopingResult (P2 AST.Program)
assignAll context (AST.Program decls m) = do
	(decls2, context) <- assignDecls context decls
	return (AST.Program decls2 m)

assignDecls :: ScopingContext -> [P2 AST.Decl] -> ScopingResult ([P2 AST.Decl], ScopingContext)
assignDecls context [] = return ([], context)
assignDecls context (decl:xs) = do
	(decl, context) <- assignDecl context decl
	(xs, context) <- assignDecls context xs
	return (decl:xs, context)

assignDecl :: ScopingContext -> P2 AST.Decl -> ScopingResult (P2 AST.Decl, ScopingContext)
assignDecl context (AST.VarDecl a ident b m) = do
	let at = snd . snd . fromJust $ idLookup ident (identifiers context)
	let m2 = annotateType at m
	y <- assignExpr b
	return (AST.VarDecl a ident y m2, context)
assignDecl context (AST.FunDecl t ident args decls stmtsin attrs m) = do
	let at = snd . snd . fromJust $ idLookup ident (identifiers context)
	let m2 = annotateType at m
	let context2 = context { types = (argsTypeContext m) ++ (types context) }
	(args, context2) <- assignFargs context2 args
	(decls, context2) <- assignVarDecls context2 decls
	let stmts = fmap (fmap (updateContext context2)) stmtsin -- iterate over the list, iterate through tree
	stmts <- assignStmts stmts
	return (AST.FunDecl t ident args decls stmts attrs m2, context { nextUniqueID = nextUniqueID context2 })
assignDecl context (AST.ExternDecl l t ident args m) = do
	let at = snd . snd . fromJust $ idLookup ident (identifiers context)
	let m2 = annotateType at m
	let context2 = context { types = (argsTypeContext m) ++ (types context) }
	(args, context2) <- assignFargs context2 args
	return (AST.ExternDecl l t ident args m2, context { nextUniqueID = nextUniqueID context2 })


-- In this part we update the context, so we pass it to he function
assignFargs :: ScopingContext -> [(P2 AST.Type, P2 AST.Identifier)] -> ScopingResult ([(P2 AST.Type, P2 AST.Identifier)], ScopingContext)
assignFargs context [] = return ([], context)
assignFargs context ((t,i):rest) = do
	let x = (fmap (updateContext context) t, fmap (updateContext context) i)
	(x, context) <- assignFarg context x
	(rest, context) <- assignFargs context rest 
	return ((x:rest), context)

assignFarg :: ScopingContext -> (P2 AST.Type, P2 AST.Identifier) -> ScopingResult ((P2 AST.Type, P2 AST.Identifier), ScopingContext)
assignFarg context (t, ident) = do
	let (at, newContext) = getAnnotatedType context t
	let newIdent = fmap (annotateType at) (AST.assignUniqueID ident (nextUniqueID context))
	let newT = fmap (annotateType at) t
	let arg = (newT, newIdent)
	let fident = (fmap forget newIdent)
	let newContext2 = addIdentifier fident Argument at newContext
	case idLookup ident (identifiers context) of
		Just (iy, (Global, _))	-> returnWithWarning (arg, newContext2) (ShadowsDeclaration fident iy Global)
		Just (iy, _)		-> returnWithError (arg, newContext) (DuplicateDeclaration fident iy)
		Nothing			-> return (arg, newContext2)

assignVarDecls :: ScopingContext -> [P2 AST.Decl] -> ScopingResult ([P2 AST.Decl], ScopingContext)
assignVarDecls context [] = return ([], context)
assignVarDecls context (xin:rest) = do
	let x = fmap (updateContext context) xin
	(x, context) <- assignVarDecl context x
	(rest, context) <- assignVarDecls context rest
	return ((x:rest), context)

assignVarDecl :: ScopingContext -> P2 AST.Decl -> ScopingResult (P2 AST.Decl, ScopingContext)
assignVarDecl context decl@(AST.VarDecl a ident b m) = do
	b <- assignExpr b
	let newIdent = AST.assignUniqueID ident (nextUniqueID context)
	let fident = (fmap forget newIdent)
	let (at, newContext) = getAnnotatedType context decl
	let newContext2 = addIdentifier fident Local at newContext
	let newDecl = AST.VarDecl a newIdent b (annotateType at m)
	case idLookup ident (identifiers context) of
		Just (iy, (Local, _))	-> returnWithError (newDecl, newContext) (DuplicateDeclaration fident iy)
		Just (iy, (scope, _))	-> returnWithWarning (newDecl, newContext2) (ShadowsDeclaration fident iy scope)
		Nothing			-> return (newDecl, newContext2)
assignVarDecl _ _ = error "COMPILER BUG: Unexpected function/extern declaration inside function body."

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
		let (_,el) = fromJust $ idLookup ident (functionLinkageContext m)
		let ident2 = setIdentInfo (IdentInfo {externLanguage=el}) ident
		exprs2 <- sequence (map assignExpr exprs)
		return $ FunCall (updateIdentifier ident2 iy) exprs2 m
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
	getAnnotatedType :: ScopingContext -> b a  -> (PolyType a, ScopingContext)

-- We can do variables and functions
instance AnnotatedType AST.Decl where
	getAnnotatedType c (VarDecl t _ b m) = getAnnotatedType c t
	getAnnotatedType c (FunDecl rt _ args _ _ _ m) = generateFunctionType c rt (map fst args) m
	getAnnotatedType c (ExternDecl _ rt _ args m) = generateFunctionType c rt (map fst args) m

generateFunctionType :: ScopingContext -> Type a -> [Type a] -> a -> (PolyType a, ScopingContext)
generateFunctionType c rt args m = (poly pc (Func argTypes (annotatedMType pc rt) m), c { types = pc, nextFTid = next })
		where
			currentFTid = nextFTid c
			allIdents = nub $ concat $ map (\t -> typeIdentifiers t []) (rt : args)
			pc = zip allIdents [currentFTid..]
			argTypes = map (annotatedMType pc) args
			next = case pc of
				[] -> currentFTid
				pc -> maximum (map snd pc) + 1

-- And just types (eg. in VarDecl or arguments of function)
instance AnnotatedType Type where
	getAnnotatedType c t = (Mono (annotatedMType pc t) (getMeta t), c { types = pc, nextFTid = next })
		where
			currentFTid = nextFTid c
			newTIs = (typeIdentifiers t []) \\ (map fst $ types c)
			pc = types c ++ zip newTIs [currentFTid..]
			next = case pc of
				[] -> currentFTid
				pc -> maximum (map snd pc) + 1
