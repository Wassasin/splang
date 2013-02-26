module SemanticAnalysis where

import AST

type Context a b = [(AST.Identifier a, b)]

idLookup :: AST.Identifier a -> Context a b -> Maybe (AST.Identifier a, b)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| AST.getIdentifierString ident == AST.getIdentifierString (fst x) = Just x
	| otherwise = idLookup ident xs

updateIdentifier :: AST.Identifier a -> AST.Identifier a -> AST.Identifier a
updateIdentifier (AST.Identifier str n a) (AST.Identifier str2 m b) = AST.Identifier str m a

data ScopingError a c = DuplicateDeclaration (AST.Identifier a) (AST.Identifier a)
	| UndeclaredIdentifier (AST.Identifier a) (Context a c)
data ScopingResult a b = Result b | Fail (ScopingError a ())

instance Monad (ScopingResult a) where
	return x = Result x
	(>>=) x f = case x of
		Fail e -> Fail e
		Result y -> f y

assignUniqueIDs :: AST.Program a -> ScopingResult a (AST.Program a)
assignUniqueIDs program = do
	(program2, context) <- assignGlobs program
	program3 <- assignAll context program2
	return program3

-- Returns a context with all top-level declarations
assignGlobs :: AST.Program a -> ScopingResult a (AST.Program a, Context a ())
assignGlobs (AST.Program decls m) = case (f decls [] 0) of
	Right (newDecls, context) -> Result (AST.Program newDecls m, context)
	Left e -> Fail e
	where
		--f :: [AST.Decl a] -> [AST.Identifier a] -> Int -> Either (ScopingError a) [AST.Decl a]
		f [] context _ = Right ([], context)
		f (x:xs) context n = do
			let ix = (AST.getIdentifier x)
			case (idLookup ix context) of
				Just (iy,_) -> Left $ DuplicateDeclaration ix iy
				Nothing -> do
					let y = case x of
						AST.VarDecl a ident b m2 ->	AST.VarDecl a (AST.assignUniqueID ix n) b m2
						AST.FunDecl a ident b c d m2 ->	AST.FunDecl a (AST.assignUniqueID ix n) b c d m2
					let rest = f xs ((ix,()):context) (n + 1)
					case rest of
						Right (xs, context2) -> Right ((y:xs), context2)
						Left e -> Left e

assignAll :: Context a () -> AST.Program a -> ScopingResult a (AST.Program a)
assignAll context (AST.Program decls m) = case sequence (map (assignDecl context) decls) of
	Fail e -> Fail e
	Result decls2 -> Result $ AST.Program decls2 m

assignDecl :: Context a () -> AST.Decl a -> ScopingResult a (AST.Decl a)
assignDecl context (AST.VarDecl a ident b m) = case assignExpr context b of
	Fail e -> Fail e
	Result y -> Result (AST.VarDecl a ident y m)
assignDecl context x = Result x

assignExpr :: Context a () -> AST.Expr a -> ScopingResult a (AST.Expr a)
assignExpr context (Var ident m)  = case idLookup ident context of
		Just (iy, _) -> Result (Var (updateIdentifier ident iy) m)
		Nothing -> Fail (UndeclaredIdentifier ident context)
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
	Nothing -> Fail (UndeclaredIdentifier ident context)
assignExpr context (Pair e1 e2 m) = do
	ee1 <- assignExpr context e1
	ee2 <- assignExpr context e2
	return $ Pair ee1 ee2 m
assignExpr context (List exprs m) = do
	exprs2 <- sequence (map (assignExpr context) exprs)
	return $ List exprs2 m
assignExpr context x = Result x


