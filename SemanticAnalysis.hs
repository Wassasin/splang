module SemanticAnalysis where

import qualified AST

type Context a = [AST.Identifier a]

idLookup :: AST.Identifier a -> Context a -> Maybe (AST.Identifier a)
idLookup ident [] = Nothing
idLookup ident (x:xs)
	| AST.getIdentifierString ident == AST.getIdentifierString x = Just x
	| otherwise = idLookup ident xs

data ScopingError a = DuplicateDeclaration (AST.Identifier a) (AST.Identifier a)
	| UndeclaredIdentifier (AST.Identifier a)
data ScopingResult a = Result (AST.Program a) | Fail (ScopingError a) 

assignGlobs :: AST.Program a -> ScopingResult a
assignGlobs (AST.Program decls m) = case (f decls [] 0) of
	Right newDecls -> Result (AST.Program newDecls m)
	Left e -> Fail e
	where
		--f :: [AST.Decl a] -> [AST.Identifier a] -> Int -> Either (ScopingError a) [AST.Decl a]
		f [] _ _ = Right []
		f (x:xs) context n = do
			let ix = (AST.getIdentifier x)
			case (idLookup ix context) of
				Just iy -> Left $ DuplicateDeclaration ix iy
				Nothing -> do
					let y = case x of
						AST.VarDecl a ident b m ->	AST.VarDecl a (AST.assignUniqueID ix n) b m
						AST.FunDecl a ident b c d mx ->	AST.FunDecl a (AST.assignUniqueID ix n) b c d m
					let rest = f xs (ix:context) (n + 1)
					case rest of
						Right xs -> Right (y:xs)
						Left e -> Left e
