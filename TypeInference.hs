{-# LANGUAGE DeriveFunctor #-}

module TypeInference (PolyType(..), MonoType(..), Substitution, InferError(..), InferContext, infer, extractContext, P3, P3Meta(..), mgu, Unification(..)) where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.List (union, (\\))
import Data.Tuple (swap)
import SemanticAnalysis (P2, P2Meta, context, stripContext, isBuiltin, typeOfBuiltin, builtinP2Meta, annotatedType)
import Errors
import Meta (ASTMeta, getMeta)
import qualified AST
import qualified Source
import Typing

data P3Meta = P3 {source :: Source.IndexSpan, inferredType :: Maybe (MonoType P2Meta)}
	deriving (Show, Eq, Read)
type P3 a = a P3Meta

promote :: P2Meta -> P3Meta
promote m = P3 {source = Source.src m, inferredType = Nothing}

fpromote :: Functor a => a P2Meta -> a P3Meta
fpromote = fmap promote

tpromote :: P2Meta -> MonoType P2Meta -> P3Meta
tpromote m t = P3 {source = Source.src m, inferredType = Just t}

type Substitution m = MonoType m -> MonoType m
data Unification m = Success (Substitution m) | Fail (MonoType m) (MonoType m)

type InferState = FTid

{- Using Void in an active place
 Annotation is wron
 Cannot unify types
 IdentID could not be found in context
 wrong number of arguemnts in function app -}
data InferError m = VoidUsage m (MonoType m) | TypeError (MonoType m) (MonoType m) | CannotUnify m (MonoType m) (MonoType m) | ContextNotFound AST.IdentID | UnknownIdentifier (AST.Identifier m) | WrongArguments [AST.Expr m] [MonoType m] m | NoFunction (AST.Identifier m) (MonoType m) m
type InferResult m a = ErrorContainer (InferError m) () (a, InferState)

type InferMonad m a = InferState -> InferResult m a
data InferMonadD m a = IM (InferMonad m a)
	deriving (Functor)

type InferFunc m a b = InferContext m -> a -> MonoType m -> InferMonadD m (b, Substitution m)

type InferContext m = AST.IdentID -> InferMonadD m (PolyType m)

bo :: InferMonadD m a -> InferMonad m a
bo (IM f) = f

mo :: InferMonad m a -> InferMonadD m a
mo f = IM f

instance Monad (InferMonadD m) where
	-- (>>=) :: InferMonadD m a -> (InferResult m a -> InferMonadD m b) -> InferMonadD m b
	(>>=) fd gm = mo $ \st1 -> do
		(a, st2) <- bo fd st1
		(b, st3) <- bo (gm a) st2
		return (b, st3)
	
	-- return :: a -> InferMonadD m a
	return a = mo $ \st -> return (a, st)
	
returnFatalInferError :: InferError m -> InferMonadD m a
returnFatalInferError e = mo $ \_ -> returnFatal e

addInferError :: InferError m -> InferMonadD m ()
addInferError e = mo $ \s -> do
	addError e
	return ((), s)

returnInferError :: a -> InferError m -> InferMonadD m a
returnInferError x e = mo $ \s -> returnWithError (x, s) e

substitute :: FreeType m -> MonoType m -> Substitution m
substitute (FT xi _) y z@(Free (FT zi _) _)	= if(xi == zi) then y else z
substitute x y z				= let s = substitute x y in case z of
		Func args r m	-> Func (map s args) (s r) m
		Pair x y m	-> Pair (s x) (s y) m
		List t m	-> List (s t) m
		x		-> x

compose :: Unification m -> Unification m -> Unification m
compose (Success x) (Success y)	= Success (x . y)
compose (Fail x y) _		= Fail x y
compose _ (Fail x y)		= Fail x y

ftvm :: MonoType m -> [FreeType m]
ftvm (Free a _)		= [a]
ftvm (Func ts t _)	= foldr (union . ftvm) (ftvm t) ts
ftvm (Pair t1 t2 _)	= union (ftvm t1) (ftvm t2)
ftvm (List t _)		= ftvm t
ftvm _			= []

ftv :: PolyType m -> [FreeType m]
ftv (Mono t _) = ftvm t
ftv (Poly a t _) = filter ((/=) a) (ftv t)

ftvc :: P2Meta -> InferContext P2Meta -> InferMonadD P2Meta ([FreeType P2Meta])
ftvc m c = do
	ts <- sequence $ map c $ stripContext $ context m
	return $ concat $ map ftv ts

mgu :: MonoType m -> MonoType m -> Unification m
mgu (Free a _) t@(Free b _)
	| a == b 			= Success id
	| otherwise			= Success (substitute a t)
mgu (Free a al) t
	| elem a (ftvm t)		= Fail (Free a al) t
	| otherwise			= Success (substitute a t)
mgu t (Free b bl)			= mgu (Free b bl) t
mgu x@(Func xs xr _) y@(Func ys yr _)
	| length xs /= length ys	= Fail x y
	| otherwise			= foldr (\(x, y) su -> case su of
							Success s	-> compose (mgu (s x) (s y)) su
							Fail x y	-> Fail x y
						) (mgu xr yr) (zip xs ys)
mgu (Pair xx xy _) (Pair yx yy _)	= case mgu xy yy of
						su@(Success s)	-> compose (mgu (s xx) (s yx)) su
						Fail x y	-> Fail x y
mgu (List x _) (List y _)		= mgu x y
mgu (Int _) (Int _)			= Success id
mgu (Bool _) (Bool _)			= Success id
mgu (Void _) (Void _)			= Success id
mgu x y					= Fail x y

setContext :: AST.IdentID -> PolyType m -> InferContext m -> InferMonadD m (InferContext m)
setContext i t c = return $ \j -> if i == j then return t else c j

extractContext :: P2Meta -> InferContext P2Meta -> InferMonadD P2Meta [(AST.IdentID, PolyType P2Meta)]
extractContext m c = sequence $ map (\i -> do
	t <- c i
	return (i, t)) $ stripContext $ context m

apply :: Substitution m -> InferContext m -> InferMonadD m (InferContext m)
apply s c = return $ \i -> do
		t <- c i
		t <- applyPoly s t
		return t

applyPoly :: Substitution m -> PolyType m -> InferMonadD m (PolyType m)
applyPoly s (Mono t m) = return $ Mono (s t) m
applyPoly s p@(Poly a t m) = case s $ Free a m of -- in case of application over bound type variable, do nothing
	(Free b _)	-> case a == b of
		False	-> return p
		True	-> do
				u <- applyPoly s t
				return (Poly a u m)
	_		-> return p

createPoly :: [FreeType m] -> MonoType m -> m -> PolyType m
createPoly as t m = foldr (\a t -> Poly a t m) (Mono t m) as

genMgu :: m -> MonoType m -> MonoType m -> InferMonadD m (Substitution m)
genMgu m t1 t2 = case mgu t1 t2 of
	Fail u1 u2 -> returnInferError id $ CannotUnify m u1 u2 -- Cannot infer; assume no substitution, and attempt to continue
	Success s -> return s

genFresh :: InferMonadD m (m -> MonoType m)
genFresh = mo $ \st -> return (\m -> Free (FT st m) m, st+1)

genFreshConcrete :: m -> InferMonadD m (MonoType m)
genFreshConcrete m = do
	a <- genFresh
	return $ a m

genBind :: m -> PolyType m -> InferMonadD m (MonoType m)
genBind _ (Mono t _) = return t
genBind m (Poly a t _) = do
	(Free b _) <- genFreshConcrete m
	t <- genBind m t
	return $ substitute a (Free b m) t

fetchIdentID :: AST.Identifier m -> InferMonadD m (AST.IdentID)
fetchIdentID (AST.Identifier _ (Just i) _)	= return i
fetchIdentID i					= returnFatalInferError $ UnknownIdentifier i

(.>) :: InferMonadD m (Substitution m) -> Substitution m -> InferMonadD m (Substitution m)
(.>) fd s2 = do
	s1 <- fd
	return $ s1 . s2

(?>) :: InferMonadD m (a, Substitution m) -> Substitution m -> InferMonadD m (a, Substitution m)
(?>) fd s2 = do
	(x, s1) <- fd
	return $ (x, s1 . s2)

emptyContext :: InferContext m
emptyContext = \i -> returnFatalInferError $ ContextNotFound i

constructInitialContext :: P2 AST.Program -> InferMonadD P2Meta (InferContext P2Meta)
constructInitialContext p = do
	let m = getMeta p
	is <- return $ stripContext $ context m
	c <- foldl (>>=) (return emptyContext) $ map (\i -> \c -> do
		if(isBuiltin i)
			then setContext i (fmap (const builtinP2Meta) $ typeOfBuiltin (toEnum i)) c
			else return c) is
	c <- constructProgramContext c p -- Validate all the global VarDecls; FunDecl validation does not do anything at this stage
	return c

constructProgramContext :: InferContext P2Meta -> P2 AST.Program -> InferMonadD P2Meta (InferContext P2Meta)
constructProgramContext c (AST.Program decls _) = do
	c <- foldl (>>=) (return c) $ map (\d -> \c -> constructDeclContext c d) decls
	return c

constructDeclContext :: InferContext P2Meta -> P2 AST.Decl -> InferMonadD P2Meta (InferContext P2Meta)
constructDeclContext c (AST.VarDecl _ i _ m) = do
	i <- fetchIdentID i
	let at = fromJust $ annotatedType m
	c <- setContext i at c
	return c
constructDeclContext c (AST.FunDecl _ i _ decls _ m) = do
	i <- fetchIdentID i
	let at = fromJust $ annotatedType m
	c <- setContext i at c
	c <- foldl (>>=) (return c) $ map (\d -> \c -> constructDeclContext c d) decls
	return c
	
validateProgramContext :: InferContext P2Meta -> P2 AST.Program -> InferMonadD P2Meta ()
validateProgramContext c (AST.Program decls _) = do
	foldl (>>=) (return ()) $ map (\d -> \() -> validateDeclContext c d) decls
	
validateDeclContext :: InferContext P2Meta -> P2 AST.Decl -> InferMonadD P2Meta ()
validateDeclContext c (AST.VarDecl _ i _ m) = do
	i <- fetchIdentID i
	at <- genBind m (fromJust $ annotatedType m)
	bt <- c i
	bt <- genBind m bt
	validateType at bt
	return ()
validateDeclContext c (AST.FunDecl _ i _ decls _ m) = do
	i <- fetchIdentID i
	at <- genBind m (fromJust $ annotatedType m)
	bt <- c i
	bt <- genBind m bt
	validateType at bt
	sequence $ map (\d -> validateDeclContext c d) decls
	return ()

validateType :: MonoType P2Meta -> MonoType P2Meta -> InferMonadD P2Meta [(FreeType P2Meta, FreeType P2Meta)]
validateType a b = vt a b []
	where
		vt :: MonoType P2Meta -> MonoType P2Meta -> [(FreeType P2Meta, FreeType P2Meta)] -> InferMonadD P2Meta [(FreeType P2Meta, FreeType P2Meta)]
		vt a@(Func aargs ar _) b@(Func bargs br _) ftmap
			| length aargs /= length bargs	= returnInferError ftmap $ TypeError a b
			| otherwise 			= do
				ftmap <- foldl (>>=) (return ftmap) $ map (\(aarg, barg) -> vt aarg barg) $ zip aargs bargs
				vt ar br ftmap
		vt (Pair aa ab _) (Pair ba bb _) ftmap = do
			ftmap <- vt aa ba ftmap
			vt ab bb ftmap
		vt (List a _) (List b _) ftmap = vt a b ftmap
		vt at@(Free a _) bt@(Free b _) ftmap = do
			case find ftmap a of
				Nothing	-> case find (map swap ftmap) b of
					Nothing	-> return $ (a, b):ftmap
					Just _	-> returnInferError ftmap $ TypeError at bt
				Just c	-> if(c == b) then return ftmap else returnInferError ftmap $ TypeError at bt
		vt (Int _) (Int _) ftmap = return ftmap
		vt (Bool _) (Bool _) ftmap = return ftmap
		vt (Void _) (Void _) ftmap = return ftmap
		vt a b ftmap = returnInferError ftmap $ TypeError a b
		
		find :: [(FreeType P2Meta, FreeType P2Meta)] -> FreeType P2Meta -> Maybe (FreeType P2Meta)
		find [] _ = Nothing
		find ((xk, xv):xs) y = if(xk == y) then Just xv else find xs y

infer :: P2 AST.Program -> InferResult P2Meta (P3 AST.Program, [(AST.IdentID, PolyType P2Meta)])
infer p = flip bo 2 $ do -- FT 0 & 1 are reserved for keywords
	c <- constructInitialContext p
	(p2, s, c) <- inferProgram c p
	validateProgramContext c p
	c2 <- extractContext (getMeta p) c
	return (fmap (f s) p2, c2)
	where
		f :: Substitution P2Meta -> P3Meta -> P3Meta
		f s m = P3 {source = source m, inferredType = case inferredType m of
			Nothing -> Nothing
			Just t -> Just $ s t}

inferProgram :: InferContext P2Meta -> P2 AST.Program -> InferMonadD P2Meta (P3 AST.Program, Substitution P2Meta, InferContext P2Meta)
inferProgram c (AST.Program decls m) = do
	(decls, s, c) <- foldl (>>=) (return ([], id, c)) $ map (\d -> \(ds, s, c) -> do
		c <- apply s c
		(d, s) <- inferDecl c d
		return (ds++[d], s, c)) decls
	c <- apply s c
	return (AST.Program decls (promote m), s, c)

inferDecl :: InferContext P2Meta -> P2 AST.Decl -> InferMonadD P2Meta (P3 AST.Decl, Substitution P2Meta)
inferDecl c (AST.VarDecl t i e m) = do
	ident <- fetchIdentID i
	(Mono a _) <- c ident
	(e, s) <- inferExpr c e a
	when (usingVoid (s a)) $ addInferError (VoidUsage m (s a))
	return (AST.VarDecl (fpromote t) (fpromote i) e (tpromote m a), s)
inferDecl ce decl@(AST.FunDecl t i args decls stmts m) = do
	ident <- fetchIdentID i
	u <- ce ident
	u <- genBind m u
	b <- if(any hasReturn stmts) then genFreshConcrete m else return $ Void m
	-- make for each argument a free type
	(argtup, args) <- unzip <$> (sequence $ map (\(t, AST.Identifier str n m) -> do
		a <- genFreshConcrete m
		return ((fromJust n, a), (fpromote t, AST.Identifier str n a))) args)
	-- set types for arguments in context
	ci <- foldl (\cf (i, t) -> cf >>= setContext i (Mono t $ getMeta t)) (return ce) argtup
	-- define vardecls
	(decls, s, ci) <- foldl (>>=) (return ([], id, ci)) $ map (\d -> \(ds, s, ci) -> do
		ci <- apply s ci
		(d, s) <- inferDecl ci d ?> s
		return (ds++[d], s, ci)) decls
	ci <- apply s ci
	-- process statements
	(stmts, s) <- foldl (>>=) (return ([], s)) $ map (\stmt -> \(stmts, s) -> do
		ci <- apply s ci
		(stmt, s) <- inferStmt ci stmt (s b) ?> s
		return (stmts++[stmt], s)) stmts
	-- define eventual type of this function
	v <- return $ Func (map (s . snd) argtup) (s b) m
	-- unify with type in original context
	s <- genMgu m (s u) (s v) .> s
	do -- validate FunDecl, because after this validation is no longer possible due to possible binding by type annotation
		fce <- ftvc m ce
		let bs = ftvm (s v) \\ fce
		v <- return $ createPoly bs (s v) m
		ce <- setContext ident v ce
		validateDeclContext ce decl
	return (AST.FunDecl (fpromote t) (fpromote i) (map (\(xarg, yarg) -> (xarg, fmap (\a -> tpromote m (s a)) yarg)) args) decls stmts (tpromote m (s v)), s)
	where
		hasReturn :: P2 AST.Stmt -> Bool
		hasReturn (AST.Scope stmts _)		= any hasReturn stmts
		hasReturn (AST.If _ _ _)		= False -- might evaluate to false, thus might not
		hasReturn (AST.IfElse _ stmtt stmte _)	= hasReturn stmtt && hasReturn stmte -- both have to contain a return
		hasReturn (AST.While _ _ _)		= False -- might evaluate to false, thus might not
		hasReturn (AST.Return _ _)		= True
		hasReturn _				= False

inferStmt :: InferFunc P2Meta (P2 AST.Stmt) (P3 AST.Stmt)
inferStmt c (AST.Expr e m) _ = do
	a <- genFreshConcrete m
	(e, s) <- inferExpr c e a
	return (AST.Expr e $ promote m, s)
inferStmt c (AST.Scope stmts m) t = do
	(stmts, s) <- foldl (>>=) (return ([], id)) $ map (\stmt -> \(stmts, s) -> do
		c <- apply s c
		(stmt, s) <- inferStmt c stmt (s t) ?> s
		return (stmts++[stmt], s)) stmts
	return (AST.Scope stmts $ promote m, s)
inferStmt c (AST.If e stmtt m) t = do
	(stmtt, s) <- inferStmt c stmtt t
	c <- apply s c
	(e, s) <- inferExpr c e (Bool $ getMeta e) ?> s
	return (AST.If e stmtt $ promote m, s)
inferStmt c (AST.IfElse e stmtt stmte m) t = do
	(stmtt, s) <- inferStmt c stmtt t
	c <- apply s c
	(stmte, s) <- inferStmt c stmte (s t) ?> s
	c <- apply s c
	(e, s) <- inferExpr c e (Bool $ getMeta e) ?> s
	return (AST.IfElse e stmtt stmte $ promote m, s)
inferStmt c (AST.While e stmt m) t = do
	(stmt, s) <- inferStmt c stmt t
	c <- apply s c
	(e, s) <- inferExpr c e (Bool $ getMeta e) ?> s
	return (AST.While e stmt $ promote m, s)
inferStmt c (AST.Assignment i e m) _ = do
	ident <- fetchIdentID i
	u <- c ident
	u <- genBind m u
	(e, s) <- inferExpr c e u
	return (AST.Assignment (fpromote i) e $ promote m, s)
inferStmt _ (AST.Return Nothing m) t = do
	s <- genMgu m t (Void m)
	return (AST.Return Nothing $ promote m, s)
inferStmt c (AST.Return (Just e) m) t = do
	a <- genFreshConcrete m
	(e, s) <- inferExpr c e a
	s <- genMgu m (s t) (s a) .> s
	return (AST.Return (Just e) $ promote m, s)

matchBinOp :: AST.BinaryOperator m -> InferMonadD m (m -> MonoType m, m -> MonoType m, m -> MonoType m)
matchBinOp (AST.Multiplication _)	= return (Int, Int, Int)
matchBinOp (AST.Division _)		= return (Int, Int, Int)
matchBinOp (AST.Modulo _)		= return (Int, Int, Int)
matchBinOp (AST.Plus _)			= return (Int, Int, Int)
matchBinOp (AST.Minus _)		= return (Int, Int, Int)
matchBinOp (AST.Cons _)			= do
						a <- genFresh
						return (a, \m -> List (a m) m, \m -> List (a m) m)
matchBinOp (AST.Equals _)		= do
						a <- genFresh
						return (a, a, Bool)
matchBinOp (AST.LesserThan _)		= return (Int, Int, Bool)
matchBinOp (AST.GreaterThan _)		= return (Int, Int, Bool)
matchBinOp (AST.LesserEqualThan _)	= return (Int, Int, Bool)
matchBinOp (AST.GreaterEqualThan _)	= return (Int, Int, Bool)
matchBinOp (AST.Nequals _)		= do
						a <- genFresh
						return (a, a, Bool)
matchBinOp (AST.And _)			= return (Bool, Bool, Bool)
matchBinOp (AST.Or _)			= return (Bool, Bool, Bool)

matchUnOp :: AST.UnaryOperator m -> InferMonadD m (m -> MonoType m, m -> MonoType m)
matchUnOp (AST.Not _)		= return (Bool, Bool)
matchUnOp (AST.Negative _)	= return (Int, Int)

inferExpr :: InferFunc P2Meta (P2 AST.Expr) (P3 AST.Expr)
inferExpr c (AST.Var i m) t = do
	ident <- fetchIdentID i
	u <- c ident
	u <- genBind m u
	s <- genMgu m t u
	return (AST.Var (fpromote i) $ tpromote m (s u), s)
inferExpr c (AST.Binop e1 op e2 m) t = do
	(xf, yf, uf) <- matchBinOp op
	let (x, y, u) = (xf $ getMeta e1, yf $ getMeta e2, uf m)
	(e1, s) <- inferExpr c e1 x
	c <- apply s c
	(e2, s) <- inferExpr c e2 (s y) ?> s
	s <- genMgu m (s t) (s u) .> s
	return (AST.Binop e1 (fpromote op) e2 $ tpromote m (s u), s)
inferExpr c (AST.Unop op e m) t = do
	(xf, uf) <- matchUnOp op
	let (x, u) = (xf $ getMeta e, uf m)
	(e, s) <- inferExpr c e x
	s <- genMgu m (s t) (s u) .> s
	return (AST.Unop (fpromote op) e $ tpromote m (s u), s)
inferExpr _ (AST.Kint x m) t = do
	u <- return $ Int m
	s <- genMgu m u t
	return (AST.Kint x $ tpromote m (s u), s)
inferExpr _ (AST.Kbool x m) t = do
	u <- return $ Bool m
	s <- genMgu m u t
	return (AST.Kbool x $ tpromote m (s u), s)
inferExpr c (AST.FunCall i es m) t = do
	ident <- fetchIdentID i
	u <- c ident
	u <- genBind m u
	case u of
		Func us _ _ -> when (length es /= length us) $ addInferError (WrongArguments es us m)
		_ -> addInferError (NoFunction i u m)
	r <- genFreshConcrete m
	as <- sequence $ map (\e -> genFreshConcrete $ getMeta e) es
	v <- return $ Func as r m
	s <- genMgu m u v
	(es, s) <- foldl (>>=) (return ([], s)) $ map (\(e, a) -> \(es, s) -> do
		c <- apply s c
		(e, s) <- inferExpr c e (s a) ?> s
		when(usingVoid (s a)) $ addInferError (VoidUsage m (s a))
		return (es++[e], s)) $ zip es as
	s <- genMgu m (s t) (s r) .> s
	return (AST.FunCall (fpromote i) es $ tpromote m (s r), s)
inferExpr c (AST.Pair e1 e2 m) t = do
	a1 <- genFreshConcrete $ getMeta e1
	(e1, s) <- inferExpr c e1 a1
	c <- apply s c
	a2 <- genFreshConcrete $ getMeta e2
	(e2, s) <- inferExpr c e2 a2 ?> s
	u <- return $ Pair a1 a2 m
	s <- genMgu m (s t) (s u) .> s
	return (AST.Pair e1 e2 $ tpromote m (s u), s)
inferExpr _ (AST.Nil m) t = do
	a <- genFreshConcrete m
	u <- return $ List a m
	s <- genMgu m t u
	return (AST.Nil $ tpromote m (s u), s)
