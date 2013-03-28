module TypePrinter (Printer, monoTypePrint, polyTypePrint, plainTypePrinter) where

import System.Console.ANSI
import Output
import Typing (MonoType, PolyType)

type Printer a = (MarkupString Styles -> a)

typePrinter :: Monad m => (String -> m a) -> (OpenClose Styles -> m a) -> MarkupString Styles -> m a
typePrinter f g [] = f ""
typePrinter f g (Left c : xs) = f [c] >> typePrinter f g xs
typePrinter f g (Right s : xs) = g s >> typePrinter f g xs

monoTypePrint :: Monad m => (Printer (m a)) -> MonoType b -> m a
monoTypePrint f t = f (outputMonoType t)

polyTypePrint :: Monad m => (Printer (m a)) -> PolyType b -> m a
polyTypePrint f t = f (outputPolyType t)

plainTypePrinter :: Printer (IO ())
plainTypePrinter = typePrinter putStr (\x -> return ())
