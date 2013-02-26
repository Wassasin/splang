module SemanticAnalysis where

import qualified AST
import qualified Typing
import qualified Data.Map as Map

type IdentId = Int
data Ident a = Var IdentId a | Func IdentId a

type IdentMap a = Map.Map String (Ident a)

data SAContext = SAC {identId :: IdentId, freeTypeId :: Typing.FTid}

incrementIdent :: SAContext -> SAContext
incrementIdent c = c {identId = (identId c) + 1}

createContext :: SAContext
createContext = SAC {identId = 0, freeTypeId = 0}

data GlobResult a = Result SAContext (IdentMap a) | Fail String (Ident a) (Ident a)

assignGlobs :: AST.Program a -> SAContext -> GlobResult a
assignGlobs (AST.Program decls _) c = f decls c Map.empty
	where
		f :: [AST.Decl a] -> SAContext -> IdentMap a -> GlobResult a
		f [] c m	= Result c m
		f (x:xs) c m	= let
			(str, ix) = createIdent x c in 
				case Map.lookup str m of
					Just iy	-> Fail str ix iy
					Nothing	-> Result (incrementIdent c) (Map.insert str ix m)
			where
				createIdent :: AST.Decl a -> SAContext -> (String, Ident a)
				createIdent (AST.VarDecl _ str _ mx) c = (str, Var (identId c) mx)
				createIdent (AST.FunDecl _ str _ _ _ mx) c = (str, Func (identId c) mx)

-- assignLocals :: AST.Program a -> SAContext -> 
-- read functions
-- read globaldecls (in order)
-- read function bodies
