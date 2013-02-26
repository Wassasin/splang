module Source where

type Index = Int
data IndexSpan = IndexSpan Index Index
	deriving (Show, Eq, Read)

beginOfSpan :: IndexSpan -> Index
beginOfSpan (IndexSpan a _) = a

type Location = (Int, Int)
data LocationSpan = LocationSpan Location Location
	deriving (Show, Eq, Read)

describe :: Location -> String -> IO ()
describe (line, col) str = putStr ("line " ++ (show (line+1)) ++ ", column " ++ (show (col+1)))
		
pointOutIndex :: Index -> String -> IO ()
pointOutIndex i str = case convert i str of
	(line, col) -> pointOutLocation (line, col) str

pointOutLocation :: Location -> String -> IO ()
pointOutLocation (line, col) str = do
	putStrLn strLine
	putStr (blank (substr (fetchLine line str) 0 col))
	putStrLn "^"
		where strLine = fetchLine line str

pointOutIndexSpan :: IndexSpan -> String -> IO ()
pointOutIndexSpan (IndexSpan from to) str
	| from == (to-1)	= pointOutIndex from str
	| otherwise		= case convert from str of
		fromL -> case convert to str of
			toL -> pointOutLocationSpan (LocationSpan fromL toL) str
			
pointOutLocationSpan :: LocationSpan -> String -> IO()
pointOutLocationSpan (LocationSpan (fLine, fCol) (tLine, tCol)) str
	| fLine < tLine = pointOutLocation (fLine, fCol) str -- Drawing span is not possible over multiple lines, fall back
	| otherwise = do
		putStrLn strLine
		putStr (blank (substr strLine 0 fCol))
		putStr "^"
		putStrLn (repeatstr (tCol - fCol - 1) '~')
		where strLine = fetchLine fLine str
		
class Span a where
	merge :: a -> a -> a

instance Span IndexSpan where
	merge (IndexSpan xx xy) (IndexSpan yx yy) = IndexSpan (min xx yx) (max xy yy)

instance Span LocationSpan where
	merge (LocationSpan xx xy) (LocationSpan yx yy) = case (xx <= yx, xy <= yy) of
		(False, False)	-> LocationSpan yx xy
		(False, True)	-> LocationSpan yx yy
		(True, False)	-> LocationSpan xx xy
		(True, True)	-> LocationSpan xx yy

convert :: Index -> String -> Location
convert n str = convert1 n str (0, 0)
	where
		convert1 :: Index -> String -> Location -> Location
		convert1 0 str l = l
		convert1 n (x:xs) (line, column) = case x of
			'\n'	-> convert1 (n-1) xs (line+1, 0)
			_	-> convert1 (n-1) xs (line, column+1)
 
fetchLine :: Int -> String -> String
fetchLine 0 str		= case findLast ((/=) '\n') str of
	Nothing -> str
	Just n -> substr str 0 (n+1)
fetchLine _ [] = []
fetchLine i ('\n':xs)	= fetchLine (i-1) xs
fetchLine i (_:xs)	= fetchLine i xs

isPrefixOf :: String -> String -> Bool
isPrefixOf (x:xs) (y:ys)	= x == y && (isPrefixOf xs ys)
isPrefixOf (x:xs) []		= False
isPrefixOf _ _			= True

findLast :: (Char -> Bool) -> String -> Maybe Index
findLast _ []		= Nothing
findLast f (x:xs)
	| not (f x)	= Nothing
	| otherwise	= case (findLast f xs) of
		Nothing	-> Just 0
		Just n	-> Just (n+1)

findstr :: String -> String -> Maybe Index
findstr _ []			= Nothing
findstr needle str
	| isPrefixOf needle str	= Just 0
	| otherwise		= case findstr needle (tail str) of
					Nothing -> Nothing
					Just n -> Just (n+1)

substr :: String -> Index -> Int -> String
substr [] _ _ = []
substr _ _ 0		= []
substr (x:xs) 0 n	= (x : substr xs 0 (n-1))
substr (x:xs) i n	= substr xs (i-1) n

precut :: String -> Index -> String
precut str 0	= str
precut (x:xs) i	= precut xs (i-1)

repeatstr :: Int -> Char -> String
repeatstr n c = take n (repeat c)

blank :: String -> String
blank = map (\c -> case c of 
		' '	-> ' '
		'\t'	-> '\t'
		_	-> ' '
	)
