module Utils where


guardJust :: String -> Maybe a -> a
guardJust str Nothing = error str
guardJust _ (Just x) = x
