module CodeGen where

import qualified AST
import qualified SSM
import ASTtoIR
import IRtoSSM

-- Eventually we only accept typed ASTs (aka: P4 AST.Program?)
generateSSM :: AST.Program a -> SSM.Program
generateSSM = irToSSM . programToIR
