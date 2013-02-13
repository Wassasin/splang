module PrettyPrinter (Printer, prettyPrinter, noPrettyPrinter, coloredPrettyPrinter, plainPrettyPrinter, prettyPrint) where

import System.Console.ANSI
import AST
import Output

type Printer a = (MarkupString Styles -> a)

prettyPrinter :: Monad m => (Char -> m a) -> (OpenClose Styles -> m a) -> MarkupString Styles -> m a
prettyPrinter f g [] = f '\n'
prettyPrinter f g (Left c : xs) = f c >> prettyPrinter f g xs
prettyPrinter f g (Right s : xs) = g s >> prettyPrinter f g xs

prettyPrint :: Monad m => (Printer (m a)) -> Program -> m a
prettyPrint f prog = f (outputProgram prog)

-- No output
noPrettyPrinter :: Printer (IO ())
noPrettyPrinter = prettyPrinter (\x -> return ()) (\x -> return ())

-- Plain Output
plainPrettyPrinter :: Printer (IO ())
plainPrettyPrinter = prettyPrinter putChar (\x -> return ())

-- Colored Output
syntaxColor :: Styles -> Color
syntaxColor Type = Cyan
syntaxColor Variable = Yellow
syntaxColor Constant = Red
syntaxColor Keyword = Black

color :: OpenClose Styles -> IO ()
color (Open s) = setSGR [SetColor Foreground Vivid (syntaxColor s)]
color (Close s) = setSGR []

coloredPrettyPrinter :: Printer (IO ())
coloredPrettyPrinter = prettyPrinter putChar color
