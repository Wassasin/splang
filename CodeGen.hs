module CodeGen where

import qualified AST
import qualified SSM
import ASTtoIR
import IRtoSSM
import TypeInference (P3)

generateSSM :: P3 AST.Program -> SSM.Program
generateSSM = irToSSM . programToIR
