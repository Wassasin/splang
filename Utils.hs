module Utils where

implode :: String -> [String] -> String
implode glue [] = ""
implode glue (x:[]) = x
implode glue (x:xs) = x ++ glue ++ (implode glue xs)

guardJust :: String -> Maybe a -> a
guardJust str Nothing = error str
guardJust _ (Just x) = x
