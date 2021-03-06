module ParserLib (Error(..), ParseFuncD(..), parse, (<|>), (<!>), many1, many, manyd1, manyd, opt, produceP1, passthrough, newObject, newObjectd, parseToken, equalsToken, parseOne) where

import qualified Source
import qualified Lexer
import Meta

type Token = Lexer.Token

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
	(NoMatch, Nothing)	-> error "COMPILER ERROR: (parse) Parser returned no match and no error!"
	(NoMatch, Just e)	-> Right e
	(Match xs, pe)		-> (case dualmap (\ i -> (case i of
			(x, (_, []))	-> Left x	-- Completely parsed, possible result
			(_, (_, t:_))	-> Right t	-- Tokens left to parse
		)) xs of
			([], ts)	-> Right (case bestMError pe (Just (foldr1 bestError (map (\ t -> Unexpected t) ts))) of -- If no options, show last token where parsing terminated
						Just e	-> e
						Nothing -> error "COMPILER BUG: (parse 2) No best error found in parser errors!")
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

(<!>) :: ParseFuncD a -> ParseFuncD a -> ParseFuncD a
(<!>) fd gd = mo (\ i -> case (bo fd) i of
	(xr, xe) -> case xr of
		Match xs	-> (Match xs, xe)
		NoMatch		-> case (bo gd) i of
			(yr, ye) -> case bestMError xe ye of
				e -> case yr of
					Match ys	-> (Match ys, e)
					NoMatch		-> (NoMatch, e))

many1 :: ParseFuncD a -> ParseFuncD [a]
many1 fd = do
	t <- fd
	do
		ts <- many1 fd
		return (t:ts)
		<|> return [t]

many :: ParseFuncD a -> ParseFuncD [a]
many fd = do
	many1 fd <|> return []
	
manyd1 :: ParseFuncD a -> ParseFuncD b -> ParseFuncD [a]
manyd1 fd gd = do
	t <- fd
	do
		gd
		ts <- manyd1 fd gd
		return (t:ts)
		<|> return [t]
	
manyd :: ParseFuncD a -> ParseFuncD b -> ParseFuncD [a]
manyd fd gd = do
	manyd1 fd gd <|> return []

opt :: ParseFuncD a -> ParseFuncD (Maybe a)
opt fd = return Nothing
	<|> do
		x <- fd
		return (Just x)

instance Monad ParseFuncD where
--	(>>=) :: ParseFuncD a -> (a -> ParseFuncD b) -> ParseFuncD b
	(>>=) fd gm = mo (\ (l0, xs) -> case (bo fd) (l0, xs) of
			(NoMatch, Nothing)-> error "COMPILER ERROR: (>>=) Parser returned no match and no error!"
			(NoMatch, Just e) -> (NoMatch, Just e)
			(Match xms, pe0) -> case dualmap (
					\(a, (l1, ys)) -> case bo (gm a) (l1, ys) of
						(NoMatch, Nothing)-> error "COMPILER ERROR: (>>= 2) Parser returned no match and no error!"
						(NoMatch, Just e1)	-> Left (bestMError pe0 (Just e1))
						(Match yms, pe1)	-> Right (map (\ (b, (l2, zs)) -> (b, (foldr1 Source.merge [l0, l1, l2], zs))) yms, bestMError pe0 pe1)
				) xms of
					(errors, []) -> (NoMatch, foldr1 bestMError errors)
					(_, metup) -> case unzip metup of
						(matches, pe2s) -> (Match (concat matches), foldr1 bestMError pe2s))

--	return :: a -> ParseFuncD a
	return a = mo (\ i -> (Match [(a, i)], Nothing))

produce :: (Source.IndexSpan -> a) -> ParseFuncD (Source.IndexSpan -> a)
produce f = mo $ \(l, xs) -> (Match [(f, (l, xs))], Nothing)

produceP1 :: (P1Meta -> a) -> ParseFuncD (Source.IndexSpan -> a)
produceP1 f = produce $ f . constructP1

passthrough :: a -> ParseFuncD (Source.IndexSpan -> a)
passthrough x = return $ \_ -> x

newObject :: ParseFuncD (Source.IndexSpan -> a) -> ParseFuncD a
newObject fd = newObjectd $ do
	f <- fd
	return $ \l -> return $ f l

newObjectd :: ParseFuncD (Source.IndexSpan -> ParseFuncD a) -> ParseFuncD a
newObjectd fd = mo $ \ (Source.IndexSpan _ _, is) ->
	let Lexer.Token _ (Source.IndexSpan from _) = head is in 
		case (bo fd) (Source.IndexSpan from from, is) of
			(NoMatch, Nothing)	-> error "COMPILER ERROR: (newObject) Parser returned no match and no error!"
			(NoMatch, Just e)	-> (NoMatch, Just e)
			(Match xs, pe0)		-> 
				case flip dualmap xs $ \(g, (Source.IndexSpan _ to, os)) ->
					let loc = Source.IndexSpan from to
					in case (bo $ g loc) (loc, os) of
						(NoMatch, Nothing)	-> error "COMPILER ERROR: (newObject 2) Parser returned no match and no error!"
						(NoMatch, Just e1)	-> Left (bestMError pe0 (Just e1))
						(Match ys, pe1)		-> Right (ys, bestMError pe0 pe1)
				of
					(errors, []) -> (NoMatch, foldr1 bestMError errors)
					(_, metup) -> case unzip metup of
						(matches, pe2s) -> (Match (concat matches), foldr1 bestMError pe2s)

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
