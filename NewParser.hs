module NewParser where

import qualified Source
import qualified Lexer
import qualified AST
import Meta

type Token = Lexer.Token Source.IndexSpan

-- AST element (P1)
-- P1A

data Error = Unexpected Token | EndOfStream
type ParseInput = (Source.IndexSpan, [Token])

data ParseResult a = Match [(a, ParseInput)] | NoMatch Error
type ParseFunc a = ParseInput -> ParseResult a
data ParseFuncD a = PF (ParseFunc a)

bo :: ParseFuncD a -> ParseFunc a
bo (PF f) = f

mo :: ParseFunc a -> ParseFuncD a
mo f = PF f

fap :: (a -> Maybe b) -> [a] -> [b]
fap f [] = []
fap f (x:xs) = case f x of
	Just y	-> y:(fap f xs)
	Nothing	-> fap f xs

dualmap :: (a -> Either b c) -> [a] -> ([b], [c])
dualmap f xs = dualmapr f xs ([], [])
	where
		dualmapr :: (a -> Either b c) -> [a] -> ([b], [c]) -> ([b], [c])
		dualmapr f [] (bs, cs)		= (bs, cs)
		dualmapr f (x:xs) (bs, cs)	= case f x of
			Left b	-> dualmapr f xs (b:bs, cs)
			Right c	-> dualmapr f xs (bs, c:cs)

bestNoMatch :: Error -> Error -> Error
bestNoMatch EndOfStream _ = EndOfStream
bestNoMatch _ EndOfStream = EndOfStream
bestNoMatch (Unexpected (Lexer.Token x (Source.IndexSpan xx xy))) (Unexpected (Lexer.Token y (Source.IndexSpan yx yy)))
	| xx <= yx	= Unexpected (Lexer.Token y (Source.IndexSpan yx yy))
	| otherwise	= Unexpected (Lexer.Token x (Source.IndexSpan xx xy))

(\/) :: ParseFunc a -> ParseFunc a -> ParseFunc a
(\/) f g = (\ i -> case (f i, g i) of
		(Match xs, Match ys)	-> Match (xs ++ ys)
		(Match xs, NoMatch _)	-> Match xs
		(NoMatch _, Match ys)	-> Match ys
		(NoMatch t, NoMatch u)	-> NoMatch (bestNoMatch t u)
	)

instance Monad ParseFuncD where
--	(>>=) :: ParseFuncD a -> (a -> ParseFuncD b) -> ParseFuncD b
	(>>=) fd gm = mo (\ i -> case (bo fd) i of
			NoMatch e -> NoMatch e
			Match xms -> case dualmap (
					\(a, (l1, ys)) -> case bo (gm a) (l1, ys) of
						NoMatch e -> Left e
						Match yms -> Right (map (\ (b, (l2, zs)) -> (b, (Source.merge l1 l2, zs))) yms)
				) xms of
					(errors, []) -> NoMatch (foldr1 bestNoMatch errors)
					(_, matchesSet) -> Match (concat matchesSet)
		)

--	return :: ASTMeta a => a -> ParseFuncD a
	return a = mo (\ i -> Match [(a, i)])

parseToken :: (Lexer.TokenE -> Bool) -> ParseFunc Lexer.TokenE
parseToken _ (l, [])	= NoMatch EndOfStream
parseToken f (_, (Lexer.Token t l:xs))
	| f t		= Match [(t, (l, xs))]
	| otherwise	= NoMatch (Unexpected (Lexer.Token t l))



-- parseBasicType :: ParseFunc (P1 AST.Type)
-- parseBasicType = mytoken (\x -> case x of
	-- Lexer.Token (Lexer.Type y) l -> Just (case y of
-- 		Lexer.Void -> AST.Void (constructP1 l)
--  		Lexer.Int -> AST.Int (constructP1 l)
-- 		Lexer.Bool -> AST.Bool (constructP1 l))
-- 	_ -> Nothing)
