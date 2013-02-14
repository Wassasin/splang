module ParserLib where

import qualified Source
import qualified Lexer
import Meta

type Token = Lexer.Token Source.IndexSpan

data Error = Unexpected Token | EndOfStream | Ambiguity
	deriving (Show, Eq, Read)
type ParseInput = (Source.IndexSpan, [Token])

data ParseResult a = Match [(a, ParseInput)] | NoMatch
type ParseFunc a = ParseInput -> (ParseResult a, Maybe Error)
data ParseFuncD a = PF (ParseFunc a)

bo :: ParseFuncD a -> ParseFunc a
bo (PF f) = f

mo :: ParseFunc a -> ParseFuncD a
mo f = PF f

parse :: ParseFuncD a -> [Token] -> Either [a] Error
parse f is = (case (bo f) (Source.IndexSpan 0 0, is) of
	(NoMatch, Just e)	-> Right e
	(Match xs, pe)		-> (case dualmap (\ i -> (case i of
			(x, (_, []))	-> Left x	-- Completely parsed, possible result
			(_, (_, t:_))	-> Right t	-- Tokens left to parse
		)) xs of
			([], ts)	-> Right (case bestMError pe (Just (foldr1 bestError (map (\ t -> Unexpected t) ts))) of -- If no options, show last token where parsing terminated
						Just e	-> e)
			(xs, _)		-> Left xs	-- Return all possible options (ideally one option)
		))

dualmap :: (a -> Either b c) -> [a] -> ([b], [c])
dualmap f xs = dualmapr f xs ([], [])
	where
		dualmapr :: (a -> Either b c) -> [a] -> ([b], [c]) -> ([b], [c])
		dualmapr f [] (bs, cs)		= (bs, cs)
		dualmapr f (x:xs) (bs, cs)	= case f x of
			Left b	-> dualmapr f xs (b:bs, cs)
			Right c	-> dualmapr f xs (bs, c:cs)

bestError :: Error -> Error -> Error
bestError EndOfStream _ = EndOfStream
bestError _ EndOfStream = EndOfStream
bestError (Unexpected (Lexer.Token x (Source.IndexSpan xx xy))) (Unexpected (Lexer.Token y (Source.IndexSpan yx yy)))
	| xy <= yy	= Unexpected (Lexer.Token y (Source.IndexSpan yx yy))
	| otherwise	= Unexpected (Lexer.Token x (Source.IndexSpan xx xy))

bestMError :: Maybe Error -> Maybe Error -> Maybe Error
bestMError (Just x) (Just y)	= Just (bestError x y)
bestMError (Just x) Nothing	= Just x
bestMError Nothing (Just y)	= Just y
bestMError Nothing Nothing	= Nothing

(<|>) :: ParseFuncD a -> ParseFuncD a -> ParseFuncD a
(<|>) fd gd = mo (\ i -> case ((bo fd) i, (bo gd) i) of
		((xr, xe), (yr, ye)) -> case bestMError xe ye of
			e -> (case (xr, yr) of
				(Match xs, Match ys)	-> (Match (xs ++ ys), e)
				(Match xs, NoMatch)	-> (Match xs, e)
				(NoMatch, Match ys)	-> (Match ys, e)
				(NoMatch, NoMatch)	-> (NoMatch, e)))

many1 :: ParseFuncD a -> ParseFuncD [a]
many1 fd = do
	t <- fd
	(many1 fd >>= return . (t:)) <|> return [t]

many :: ParseFuncD a -> ParseFuncD [a]
many fd = do
	many1 fd <|> return []

instance Monad ParseFuncD where
--	(>>=) :: ParseFuncD a -> (a -> ParseFuncD b) -> ParseFuncD b
	(>>=) fd gm = mo (\ (l0, xs) -> case (bo fd) (l0, xs) of
			(NoMatch, Just e) -> (NoMatch, Just e)
			(Match xms, pe0) -> case dualmap (
					\(a, (l1, ys)) -> case bo (gm a) (l1, ys) of
						(NoMatch, Just e1)	-> Left (bestMError pe0 (Just e1))
						(Match yms, pe1)	-> Right (map (\ (b, (l2, zs)) -> (b, (foldr1 Source.merge [l0, l1, l2], zs))) yms, bestMError pe0 pe1)
				) xms of
					(errors, []) -> (NoMatch, foldr1 bestMError errors)
					(_, metup) -> case unzip metup of
						(matches, pe2s) -> (Match (concat matches), foldr1 bestMError pe2s))

--	return :: a -> ParseFuncD a
	return a = mo (\ i -> (Match [(a, i)], Nothing))

produce :: (Source.IndexSpan -> a) -> ParseFuncD a
produce f = mo (\ (l, xs) -> (Match [(f l, (l, xs))], Nothing))

produceP1 :: (P1Meta -> a) -> ParseFuncD a
produceP1 f = produce (f . constructP1)

newObject :: ParseFuncD a -> ParseFuncD a
newObject fd = mo (\ (Source.IndexSpan from to, is) -> case (bo fd) (Source.IndexSpan to to, is) of
		(NoMatch, Just e)	-> (NoMatch, Just e)
		(Match xs, pe0)		-> case map (\output -> case output of
				(x, (Source.IndexSpan _ rto, os)) -> (x, (Source.IndexSpan from rto, os))
			) xs of
				matches -> (Match matches, pe0))

parseToken :: (Lexer.TokenE -> Bool) -> ParseFuncD Lexer.TokenE
parseToken f = mo (\ i -> case i of
		(_, [])				-> (NoMatch, Just EndOfStream)
		(il, (Lexer.Token t l:xs))	-> case f t of
			True -> (Match [(t, (Source.merge il l, xs))], Nothing)
			False -> (NoMatch, Just (Unexpected (Lexer.Token t l))))
	
equalsToken :: Lexer.TokenE -> ParseFuncD Lexer.TokenE
equalsToken t = parseToken ((==) t)

parseOne :: (Token -> Maybe a) -> ParseFuncD a
parseOne f = mo (\ i -> case i of
		(_, [])				-> (NoMatch, Just EndOfStream)
		(il, (Lexer.Token t l):xs)	-> case f (Lexer.Token t l) of
			Nothing -> (NoMatch, Just (Unexpected (Lexer.Token t l)))
			Just result -> (Match [(result, (Source.merge il l, xs))], Nothing))
