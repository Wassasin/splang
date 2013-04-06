module Output (Styles(..), OpenClose(..), Markup, MarkupString, OutputInfo, lift, basicInfo, withDeclCommentLine, Output(..)) where

import AST
import qualified Typing

data Styles = Type
	| Variable
	| Constant
	| Keyword
	| Function
	| UniqueID
	| Comments

data OpenClose a = Open a | Close a
type Markup a = Either Char (OpenClose a)
type MarkupString a = [Markup a]

data OutputInfo a = OutputInfo
	{ declComment :: Decl a -> MarkupString Styles
	, indentation :: Int
	, brackets :: Bool }

lift :: String -> MarkupString a
lift = fmap Left

erase :: MarkupString a -> String
erase [] = []
erase (Left c : xs) = c : erase xs
erase (Right _: xs) = erase xs

open :: a -> MarkupString a
open a = return $ Right $ Open a

close :: a -> MarkupString a
close a = return $ Right $ Close a

getIdentifierUniqueID :: Identifier a -> String
getIdentifierUniqueID (Identifier _ (Just n) _) = show n
getIdentifierUniqueID (Identifier _ Nothing _) = ""

keyword str = open Keyword ++ lift str ++ close Keyword
constant str = open Constant ++ lift str ++ close Constant
variable ident = open Variable ++ lift (getIdentifierString ident) ++ close Variable ++ open UniqueID ++ lift (getIdentifierUniqueID ident) ++ close UniqueID
function ident = open Function ++ lift (getIdentifierString ident) ++ close Function ++ open UniqueID ++ lift (getIdentifierUniqueID ident) ++ close UniqueID

basicInfo :: OutputInfo a
basicInfo = OutputInfo
	{ declComment = (\_ -> lift "")
	, indentation = 0
	, brackets = False }

indent :: OutputInfo a -> OutputInfo a
indent o = o { indentation = 1 + indentation o }

withBrackets :: OutputInfo a -> OutputInfo a
withBrackets o = o { brackets = True }

withoutBrackets :: OutputInfo a -> OutputInfo a
withoutBrackets o = o { brackets = False }

withDeclCommentLine :: (Decl a -> MarkupString Styles) -> OutputInfo a -> OutputInfo a
withDeclCommentLine f o = o { declComment = (\d -> open Comments ++ lift "// " ++ f d ++ close Comments ++ lift "\n") }

join :: (a -> [b]) -> [b] -> [a] -> [b]
join _ _ [] = []
join f _ [x] = f x
join f s (x:xs) = f x ++ s ++ join f s xs

delim :: (a -> [b]) -> [b] -> [a] -> [b]
delim _ _ [] = []
delim f s (x:xs) = f x ++ s ++ delim f s xs

tabs :: OutputInfo a -> MarkupString Styles
tabs x = lift (take (indentation x) (repeat '\t'))

class Output b where
	output :: OutputInfo a -> b a -> MarkupString Styles

instance Output Program where
	output mo (Program pr _)		= join (output mo) (lift "\n\n") pr

instance Output Decl where
	output mo decl@(VarDecl t i e _)	= tabs mo ++ (declComment mo decl) ++ tabs mo ++ output mo t ++ lift " " ++ variable i ++ lift " = " ++ output (withoutBrackets mo) e ++ lift ";"
	output mo decl@(FunDecl t i args vdecls stmts _) = tabs mo ++ (declComment mo decl) ++ tabs mo ++ output mo t ++ lift " " ++ function i ++ lift "(" ++ join (outputArg mo) (lift ", ") args ++ lift "){\n" ++ delim (output (indent mo)) (lift "\n") vdecls ++ delim (output (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"

outputArg :: OutputInfo a -> (Type a, Identifier a) -> MarkupString Styles
outputArg mo (t, i) = output mo t ++ lift " " ++ variable i

instance Output Type where
	output mo t = open Type ++ lift (outputType' t) ++ close Type where
		outputType' (Void _)			= "Void"
		outputType' (Int _)			= "Int"
		outputType' (Bool _)			= "Bool"
		outputType' (TypeIdentifier i _)	= getIdentifierString i
		outputType' (Product t1 t2 _)		= "(" ++ erase (output mo t1) ++ ", " ++ erase (output mo t2) ++ ")"
		outputType' (ListType t _)		= "[" ++ erase (output mo t) ++ "]"

instance Output Stmt where
	output mo (Expr e _)				= tabs mo ++ output (withoutBrackets mo) e ++ lift ";"
	output mo (Scope [] _)				= tabs mo ++ lift "{}"
	output mo (Scope stmts _)			= tabs mo ++ lift "{\n" ++ delim (output (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"
	output mo (If e stmt _)				= tabs mo ++ keyword "if" ++ lift "(" ++ output (withoutBrackets mo) e ++ lift ")" ++ rest mo stmt
	output mo (IfElse e s1 s2 _)			= tabs mo ++ keyword "if" ++ lift "(" ++ output (withoutBrackets mo) e ++ lift ")" ++ rest mo s1 ++ between ++ rest mo s2
		where between = if isBlock s1
			then keyword " else "
			else lift "\n" ++ tabs mo ++ keyword "else"
	output mo (While e stmt _)			= tabs mo ++ keyword "while" ++ lift "(" ++ output (withoutBrackets mo) e ++ lift ")" ++ rest mo stmt
	output mo (Assignment i e _)			= tabs mo ++ variable i ++ lift " = " ++ output (withoutBrackets mo) e ++ lift ";"
	output mo (Return (Just e) _)			= tabs mo ++ keyword "return " ++ output (withoutBrackets mo) e ++ lift ";"
	output mo (Return Nothing _)			= tabs mo ++ keyword "return" ++ lift ";"

-- Exception for scope after if/else/while
rest :: OutputInfo a -> Stmt a -> MarkupString Styles
rest mo stmt = case stmt of
	(Scope stmts _) -> lift "{\n" ++ delim (output (indent mo)) (lift "\n") stmts ++ tabs mo ++ lift "}"
	y -> lift "\n"  ++ output (indent mo) y

isBlock :: Stmt a -> Bool
isBlock (Scope _ _) = True
isBlock _ = False

enclose :: OutputInfo a -> MarkupString Styles -> MarkupString Styles
enclose o str
	| brackets o = lift "(" ++ str ++ lift ")"
	| otherwise  = str

instance Output Expr where
	output mo (Var i _) 			= variable i
	output mo (Binop e1 bop e2 _)		= enclose mo $ output (withBrackets mo) e1 ++ output mo bop ++ output (withBrackets mo) e2
	output mo (Unop uop e _)		= enclose mo $ output mo uop ++ output (withBrackets mo) e
	output mo (Kint n _)			= constant (show n)
	output mo (Kbool b _)			= constant (show b)
	output mo (FunCall i exprs _)		= function i ++ lift "(" ++ join (output (withoutBrackets mo)) (lift ", ") exprs ++ lift ")"
	output mo (Pair e1 e2 _)		= lift "(" ++ output (withoutBrackets mo) e1 ++ lift ", " ++ output (withoutBrackets mo) e2 ++ lift ")"
	output mo (Nil _)			= lift "[]"

instance Output BinaryOperator where
	output mo (Multiplication _)		= lift " * "
	output mo (Division _)			= lift " / "
	output mo (Modulo _)			= lift " % "
	output mo (Plus _)			= lift " + "
	output mo (Minus _)			= lift " - "
	output mo (Cons _)			= lift " : "
	output mo (Equals _)			= lift " == "
	output mo (LesserThan _)		= lift " < "
	output mo (GreaterThan _)		= lift " > "
	output mo (LesserEqualThan _)		= lift " <= "
	output mo (GreaterEqualThan _)		= lift " >= "
	output mo (Nequals _)			= lift " != "
	output mo (And _)			= lift " && "
	output mo (Or _)			= lift " || "

instance Output UnaryOperator where
	output mo (Not _) 			= lift "!"
	output mo (Negative _)			= lift "-"

instance Output Typing.FreeType where
	output mo (Typing.FT i _)		= open Variable ++ lift "a" ++ close Variable ++ open UniqueID ++ lift (show i) ++ close UniqueID

instance Output Typing.MonoType where
	output mo (Typing.Func args r _)	= (output mo r) ++ lift "(" ++ join (output mo) (lift ", ") args ++ lift ")"
	output mo (Typing.Pair x y _)		= lift "(" ++ output mo x ++ lift ", " ++ output mo y ++ lift ")"
	output mo (Typing.List t _)		= lift "[" ++ output mo t ++ lift "]"
	output mo (Typing.Free t _)		= output mo t
	output mo (Typing.Int _)		= open Type ++ lift "Int" ++ close Type
	output mo (Typing.Bool _)		= open Type ++ lift "Bool" ++ close Type
	output mo (Typing.Void _)		= open Type ++ lift "Void" ++ close Type

instance Output Typing.PolyType where
	output mo (Typing.Poly f t _)		= open Keyword ++ lift "forall " ++ close Keyword ++ output mo f ++ lift " . " ++ output mo t
	output mo (Typing.Mono t _)		= output mo t
