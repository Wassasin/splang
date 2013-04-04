module Errors (ErrorContainer(..), addWarning, returnWithWarning, addError, returnWithError, returnFatal) where

-- errors, warnings and data
data ErrorContainer e w d = Result d [e] [w] | FatalError e [e] [w]

-- monad structure to pass the errors around
instance Monad (ErrorContainer e w) where
	return x = Result x [] []
	(>>=) (Result y errors warnings) f = case f y of
		Result z errors2 warnings2 -> Result z (errors ++ errors2) (warnings ++ warnings2)
		FatalError ferror errors2 warnings2 -> FatalError ferror (errors ++ errors2) (warnings ++ warnings2)
	(>>=) (FatalError fe es ws) f = FatalError fe es ws

addWarning :: w -> ErrorContainer e w ()
addWarning w = Result () [] [w]

returnWithWarning :: d -> w -> ErrorContainer e w d
returnWithWarning x w = Result x [] [w]

addError :: e -> ErrorContainer e w ()
addError e = Result () [e] []

returnWithError :: d -> e -> ErrorContainer e w d
returnWithError x e = Result x [e] []

returnFatal :: e -> ErrorContainer e w d
returnFatal e = FatalError e [] []
