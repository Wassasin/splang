{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Templating (template) where

import Data.Traversable as Trav
import Control.Monad.State
import Control.Monad.Writer

import Typing (MonoType(..))
import SemanticAnalysis (P2, P2Meta(..))
import TypeInference (P3, P3Meta(..), Substitution)
import Meta (getMeta)
import Builtins (isBuiltin)
import Utils

import qualified AST
import qualified ASTWalker
import qualified Typing
import qualified TypeInference

type FunctionKey = (AST.IdentID, P2 Typing.MonoType)
type FunctionNameMap = FunctionKey -> Maybe AST.IdentID
type FunctionDeclMap = AST.IdentID -> P3 AST.Decl

data TemplateState = TemplateState {nameMap :: FunctionNameMap, queue :: [FunctionKey], nextUniqueID :: AST.IdentID}
type TemplateMonad a = State TemplateState a

whileM :: (s -> Bool) -> State s a -> State s [a]
whileM f m = do
	state <- get
	if f state
	then do
		x <- m
		xs <- whileM f m
		return $ x : xs
	else return []

append :: FunctionKey -> AST.IdentID -> FunctionNameMap -> FunctionNameMap
append kx v f = \ky -> if(kx == ky) then Just v else (f kx)

newState :: AST.Program m -> TemplateState
newState p = TemplateState {nameMap = \_ -> Nothing, queue = [], nextUniqueID = maxIdentID p + 1}

appendToQueue :: FunctionKey -> TemplateMonad ()
appendToQueue key = do
	state <- get
	put $ state { queue = (queue state) ++ [key] }

iterateIdentID :: TemplateMonad AST.IdentID
iterateIdentID = do
	state <- get
	let newid = nextUniqueID state
	put $ state { nextUniqueID = newid + 1 }
	return newid

template :: AST.Program P3Meta -> AST.Program P3Meta
template p@(AST.Program decls m) = flip evalState (newState p) $ do
	let fm = getMeta $ guardJust "COMPILER BUG: fm is not set" $ inferredType m
	appendToQueue $ (findMain decls, Func [] (Void fm) fm)
	varDecls <- templateVarDecls decls
	funDecls <- templateFunDecls $ createFunDeclMap decls
	return $ AST.Program (varDecls ++ funDecls) m
	where
		findMain :: [P3 AST.Decl] -> AST.IdentID
		findMain [] = error "Program error: function main not found" -- TODO
		findMain (AST.VarDecl _ _ _ _ : decls) = findMain decls
		findMain (AST.FunDecl _ (AST.Identifier str (Just id) _) _ _ _ _ : decls) = if str == "main" then id else findMain decls
	
		templateVarDecls :: [P3 AST.Decl] -> TemplateMonad [P3 AST.Decl]
		templateVarDecls decls = Trav.mapM (rewrite id) $ filter AST.isVarDecl decls -- rewrite all global vardecls for concrete types

		templateFunDecls :: FunctionDeclMap -> TemplateMonad [P3 AST.Decl]
		templateFunDecls declmap = whileM (not . null . queue) $ do
			state <- get
			let key@(id, t):keys = queue state -- is always non-empty due to whileM-condition
			modify (\state -> state { queue = tail $ queue $ state }) -- remove top from queue
			let Just newid = (nameMap state) key
			let AST.FunDecl ftd (AST.Identifier fstr (Just fid) fim) fargs fdecls fstmts fm = declmap id
			let s = case TypeInference.mgu t $ guardJust "mgu" $ inferredType $ fm of
				TypeInference.Success s -> s
				_ -> error "Can not infer"
			rewrite s $ AST.FunDecl ftd (AST.Identifier (mangle (fstr, fid, t)) (Just newid) fim) fargs fdecls fstmts fm

		createFunDeclMap :: [P3 AST.Decl] -> FunctionDeclMap
		createFunDeclMap [] = \_ -> error "COMPILER BUG: Referenced to non-existant function"
		createFunDeclMap (AST.VarDecl _ _ _ _ : decls) = createFunDeclMap decls
		createFunDeclMap (decl@(AST.FunDecl _ (AST.Identifier _ (Just id) _) _ _ _ _) : decls) = \x -> if x == id then decl else (createFunDeclMap decls) x

class ASTWalker.ASTWalker a => Templateable a where
	rewrite :: Substitution P2Meta -> a P3Meta -> TemplateMonad (a P3Meta)
	rewrite s x = ASTWalker.walk (return, return, return, return, fe s, return) x
		where
			fe :: Substitution P2Meta -> AST.Expr P3Meta -> TemplateMonad (AST.Expr P3Meta)
			fe s decl@(AST.FunCall (AST.Identifier istr (Just iid) im) exprs m) =
				if isBuiltin iid
				then
					return decl
				else do
					let exprst = map (s . (guardJust "exprst") . inferredType . getMeta) exprs
					let rt = s $ guardJust "COMPILER BUG: rt is not set" $ inferredType m 
					let t = Typing.Func exprst rt $ getMeta $ guardJust "COMPILER BUG: t is not set" $ inferredType m
					let key = (iid, t)
					state <- get
					newid <- case (nameMap state) key of
						Just newid -> return newid
						Nothing -> do
							appendToQueue key -- queue function
							newid <- iterateIdentID -- generate uniqueID and iterate
							modify $ \state -> state { nameMap = append key newid $ nameMap state } -- add mapping of (id, type) to newid
							return newid
					return $ AST.FunCall (AST.Identifier (mangle (istr, iid, t)) (Just newid) im) exprs m
			fe _ e = do
				return e
	
	maxIdentID :: a m -> AST.IdentID
	maxIdentID x = maximum $ snd $ runWriter $ ASTWalker.walk (return, return, return, return, return, fi) x
		where
			fi :: AST.Identifier m -> Writer [AST.IdentID] (AST.Identifier m)
			fi i@(AST.Identifier _ (Just id) _) = do
				tell [id]
				return i

-- gen concrete code
instance Templateable AST.Program
instance Templateable AST.Decl

class Mangle a where
	mangle :: a -> String
	
instance Mangle (String, AST.IdentID, MonoType a) where
	mangle (str, i, t) = str ++ show i ++ "_" ++ mangle t

instance Mangle (MonoType a) where
	mangle (Typing.Func args r _)	= (mangle r) ++ "_" ++ (concat $ map mangle args)
	mangle (Typing.Pair x y _)	= "p" ++ (mangle x) ++ (mangle y)
	mangle (Typing.List (Typing.Free _ _) _)	= "lf"
	mangle (Typing.List x _)	= "l" ++ mangle x
	mangle (Typing.Free _ _)	= error "COMPILER BUG: Can not translate an abstract type to a concrete datatype"
	mangle (Typing.Int _)		= "i"
	mangle (Typing.Bool _)		= "b"
	mangle (Typing.Void _)		= "v"
