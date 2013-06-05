module CodeGen where

import qualified AST
import qualified IR
import qualified SSM
import qualified LLVM
import ASTtoIR
import IRDeadcode
import IRtoSSM
import IRtoLLVM
import TypeInference (P3)

-- Basic Steps
toIR :: P3 AST.Program -> IR.Program IR.IRStmt
toIR = translateProgram

canonicalizeIR :: IR.Program IR.IRStmt -> IR.Program [IR.BasicBlock]
canonicalizeIR = IRDeadcode.optimize . IR.linearize

toSSM :: IR.Program [IR.BasicBlock] -> SSM.Program
toSSM = irToSSM

toLLVM :: IR.Program [IR.BasicBlock] -> LLVM.Program
toLLVM = irToLLVM

-- Full steps
generateSSM :: P3 AST.Program -> SSM.Program
generateSSM = toSSM . canonicalizeIR . toIR

generateLLVM :: P3 AST.Program -> LLVM.Program
generateLLVM = toLLVM . canonicalizeIR . toIR
