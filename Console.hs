module Console where

import System.Console.ANSI
import qualified Source

data MessageE = Error | Warning | Note

colorForMessageE :: MessageE -> Color
colorForMessageE Error = Red
colorForMessageE Warning = Magenta
colorForMessageE Note = Black

stringForMessageE :: MessageE -> String
stringForMessageE Error = "error: "
stringForMessageE Warning = "warning: "
stringForMessageE Note = "note: "

putMessage :: MessageE -> String -> String -> Source.Location -> String -> IO ()
putMessage kind file s loc err = do
	setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
	putStr (file ++ ":" ++ show (fst loc) ++ ":" ++ show (snd loc) ++ ": ")
	setSGR [SetColor Foreground Vivid (colorForMessageE kind), SetConsoleIntensity BoldIntensity]
	putStr (stringForMessageE kind)
	setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
	putStr err
	setSGR []
	putStr "\n"

highLight :: String -> IO ()
highLight str = do
	setSGR [SetColor Foreground Vivid Yellow, SetColor Background Dull Red, SetConsoleIntensity BoldIntensity]
	putStr str
	setSGR []
	putStr "\n"
