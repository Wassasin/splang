{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Templating where

import Data.Traversable as Trav
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromJust)

import Typing (MonoType)
import SemanticAnalysis (P2, P2Meta(..))
import TypeInference (P3, P3Meta(..), Substitution)
import Meta (getMeta)

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

--template :: AST.Program P3Meta -> AST.Program P3Meta
--template p = flip evalState emptyState . execWriterT . templateF
--where
	-- create map
	-- find Void main(), add to queue
	-- until queue is empty
		-- rewrite

templateVarDecls :: [P3 AST.Decl] -> TemplateMonad [P3 AST.Decl]
templateVarDecls decls = Trav.mapM (rewrite id) $ filter AST.isVarDecl decls -- rewrite all global vardecls for concrete types

templateFunDecls :: FunctionDeclMap -> TemplateMonad [P3 AST.Decl]
templateFunDecls declmap = whileM (null . queue) $ do
	state <- get
	let key@(id, t):keys = queue state -- is always non-empty due to whileM-condition
	let Just newid = (nameMap state) key
	let AST.FunDecl ftd (AST.Identifier fstr (Just fid) fim) fargs fdecls fstmts fm = declmap id
	let TypeInference.Success s = TypeInference.mgu t $ fromJust $ inferredType $ fm
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
			fe s (AST.FunCall (AST.Identifier istr (Just iid) im) exprs m) = do
				let exprst = map (s . fromJust . inferredType . getMeta) exprs
				let rt = s $ fromJust $ inferredType im 
				let t = Typing.Func exprst rt $ getMeta $ fromJust $ inferredType im
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
			fe s e = do
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
	mangle (Typing.Func args r _)	= "r" ++ (mangle r) ++ (concat $ map (('a':).mangle) args)
	mangle (Typing.Pair x y _)	= "p" ++ (mangle x) ++ (mangle y)
	mangle (Typing.List (Typing.Free _ _) _)	= "lf"
	mangle (Typing.List x _)	= "l" ++ mangle x
	mangle (Typing.Free _ _)	= error "COMPILER BUG: Can not translate an abstract type to a concrete datatype"
	mangle (Typing.Int _)		= "i"
	mangle (Typing.Bool _)		= "b"
	mangle (Typing.Void _)		= "v"
