module Console (MessageE(..), putMessage, putMessageLn, highLight, highLightLn, intense) where

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

putMessage :: MessageE -> String -> Source.Location -> String -> IO ()
putMessage kind file loc err = do
	setSGR [SetConsoleIntensity BoldIntensity]
	putStr (file ++ ":" ++ show (1 + fst loc) ++ ":" ++ show (1 + snd loc) ++ ": ")
	setSGR [SetColor Foreground Vivid (colorForMessageE kind), SetConsoleIntensity BoldIntensity]
	putStr (stringForMessageE kind)
	setSGR [Reset, SetConsoleIntensity BoldIntensity]
	putStr err
	setSGR []

putMessageLn :: MessageE -> String -> Source.Location -> String -> IO ()
putMessageLn kind file loc err = do
	putMessage kind file loc err
	putStr "\n"

highLight :: String -> IO ()
highLight str = do
	setSGR [SetColor Foreground Vivid Yellow, SetColor Background Dull Red, SetConsoleIntensity BoldIntensity]
	putStr str
	setSGR []

highLightLn :: String -> IO ()
highLightLn str = do
	highLight str
	putStr "\n"

intense :: String -> IO ()
intense str = do
	setSGR [Reset, SetConsoleIntensity BoldIntensity]
	putStr str
	setSGR []
