module CodeGen where

import qualified AST
import qualified IR
import qualified SSM
import ASTtoIR
import IRtoSSM
import TypeInference (P3)

-- Basic Steps
toIR :: P3 AST.Program -> IR.Program IR.IRStmt
toIR = translateProgram

canonicalizeIR :: IR.Program IR.IRStmt -> IR.Program [IR.BasicBlock]
canonicalizeIR = IR.linearize

toSSM :: IR.Program [IR.BasicBlock] -> SSM.Program
toSSM = irToSSM

-- Full steps
generateSSM :: P3 AST.Program -> SSM.Program
generateSSM = irToSSM . canonicalizeIR . toIR
