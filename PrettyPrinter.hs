module PrettyPrinter (Printer, prettyPrinter, miniPrettyPrinter, coloredPrettyPrinter, plainPrettyPrinter, prettyPrint) where

import System.Console.ANSI
import AST
import Output

type Printer a = (MarkupString Styles -> a)

prettyPrinter :: Monad m => (Char -> m a) -> (OpenClose Styles -> m a) -> MarkupString Styles -> m a
prettyPrinter f g [] = f '\n'
prettyPrinter f g (Left c : xs) = f c >> prettyPrinter f g xs
prettyPrinter f g (Right s : xs) = g s >> prettyPrinter f g xs

prettyPrint :: Monad m => OutputInfo b -> (Printer (m a)) -> Program b -> m a
prettyPrint info f prog = f (output info prog)

-- Plain Output
plainPrettyPrinter :: Printer (IO ())
plainPrettyPrinter = prettyPrinter putChar (const (return ()))

-- minizer output
stripy '\t' = return ()
stripy '\n' = return ()
stripy c = putChar c

miniPrettyPrinter :: Printer (IO ())
miniPrettyPrinter = prettyPrinter stripy (const (return ()))

-- Colored Output
syntaxColor :: Styles -> Color
syntaxColor Type = Cyan
syntaxColor Variable = Yellow
syntaxColor Constant = Red
syntaxColor Keyword = Blue
syntaxColor Function = Green
syntaxColor UniqueID = Magenta
syntaxColor Comments = Black

color :: OpenClose Styles -> IO ()
color (Open s) = setSGR [SetColor Foreground Vivid (syntaxColor s)]
color (Close s) = setSGR []

coloredPrettyPrinter :: Printer (IO ())
coloredPrettyPrinter = prettyPrinter putChar color
