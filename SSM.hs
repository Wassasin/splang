{-# LANGUAGE TemplateHaskell #-}

module SSM where

import Data.DeriveTH

type Label = String
data Register = PC | SP | MP | RR
	deriving (Show)

data Instruction
	= Label Label				-- Not an instruction, but can be placed in ssm
	| Add					-- add; Addition. Replaces 2 top stack values with the addition of those values.
	| AdjustStack Int			-- ajs; Adjust Stack. Adjusts the stackpointer with fixed amount.
	| And					-- and; And. Replaces 2 top stack values with the bitwise and of those values.
	-- | Annote Int Int Int Color String
	| BranchAlways Label			-- bra; Branch Always. Jumps to the destination. Replaces the PC with the destination address.
	| BranchOnFalse Label			-- brf; Branch on False. If a False value is on top of the stack, jump to the destination.
	| BranchOnTrue Label			-- brt; Branch on True. If a True value is on top of the stack, jump to the destination.
	| BranchToSubroutine Label		-- bsr; Branch to subroutine. Pushes the PC on the stack and jumps to the subroutine.
	| Divide				-- div; Division. Replaces 2 top stack values with the division of those values.
	| Equal					-- eq; Test for equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with beq.
	| GreaterEqual				-- ge; Test for greater or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bge.
	| GreaterThan				-- gt; Test for greater then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bgt.
	| Halt					-- halt; Halt execution. Machine stops executing instructions.
	| JumpToSubroutine			-- jsr; Jump to subroutine. Pops a destination from the stack, pushes the PC on the stack and jumps to the destination.
	| LoadViaAddress Int			-- lda; Load via Address. Dereferencing. Pushes the value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
	| LoadAddressOfAddress Int		-- ldaa; Load Address of Address. Pushes the address of a value relative to the address on top of the stack. This instruction effectively adds a constant to the top of the stack.
	| LoadConstant Int			-- ldc; Load Constant. Pushes the inline constant on the stack.
	| LoadLocal Int				-- ldl; Load Local. Pushes a value relative to the markpointer.
	| LoadLocalAddress Int			-- ldla; Load Local Address. Pushes the address of a value relative to the markpointer.
	| LoadMultipleViaAddress Int Int	-- ldma; Load Multiple via Address. Pushes values relative to by the value at the top of the stack. Same as single load variant but second inline parameter is size.
	| LoadMultipleLocal Int Int		-- ldml; Load Multiple Local. Pushes values relative to the markpointer. Same as single load variant but second inline parameter is size.
	| LoadMultipleFromStack Int Int		-- ldms; Load Multiple from Stack. Pushes values relative to the top of the stack. Same as single load variant but second inline parameter is size.
	| LoadRegister Register			-- ldr; Load Register. Pushes a value from a register. Registers 0, 1, 2 and 3 are called PC (program counter), SP (stack pointer), MP (mark pointer) and RR (return register) respectively.
	| LoadRegisterFromRegister Register Register -- ldrr; Load Register from Register. Copy the content of the second register to the first. Does not affect the stack.
	| LoadFromStack Int			-- lds; Load from Stack. Pushes a value relative to the top of the stack.
	| LoadStackAddress Int			-- ldsa; Load Stack Address. Pushes the address of a value relative to the stackpointer.
	| LesserEqual				-- le; Test for less or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with ble.
	| Link Int				-- link; Reserve memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
	| LesserThan				-- lt; Test for less then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with blt.
	| Modulo				-- mod; Division. Replaces 2 top stack values with the modulo of those values.
	| Multiply				-- mul; Multiplication. Replaces 2 top stack values with the multiplication of those values.
	| NotEqual				-- ne; Test for not equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bne.
	| Negation				-- neg; Negation. Replaces top stack values with the (integer) negative of the value.
	| NoOperation				-- nop; No operation. Well, guess what...
	| Not					-- not; Not. Replaces top stack values with the bitwise complement of the value.
	| Or					-- or; Or. Replaces 2 top stack values with the bitwise or of those values.
	| Return				-- ret; Return from subroutine. Pops a previously pushed PC from the stack and jumps to it.
	| StoreViaAddress Int			-- sta; Store via Address. Pops 2 values from the stack and stores the second popped value in the location pointed to by the first. The pointer value is offset by a constant offset.
	| StoreLocal Int			-- stl; Store Local. Pops a value from the stack and stores it in a location relative to the markpointer.
	| StoreMultipleViaAddress Int Int	-- stma; Store Multiple via Address. Pops values from the stack and stores it in a location relative to the value at the top of the stack. Same as single store variant but second inline parameter is size.
	| StoreMultipleLocal Int Int		-- stml; Store Multiple Local. Pops values from the stack and stores it in a location relative to the markpointer. Same as single store variant but second inline parameter is size.
	| StoreMultipleIntoStack Int Int	-- stms; Store Multiple into Stack. Pops values from the stack and stores it in a location relative to the top of the stack. Same as single store variant but second inline parameter is size.
	| StoreRegister Register		-- str; Store Register. Pops a value from the stack and stores it in a location relative to the markpointer. See also ldr.
	| StoreIntoStack Int			-- sts; Store into Stack. Pops a value from the stack and stores it in a location relative to the top of the stack.
	| Substraction				-- sub; Substraction. Replaces 2 top stack values with the subtraction of those values.
	| SwapValues				-- swp; Swap values. Swaps the 2 topmost values on the stack.
	| SwapRegister Register			-- swpr; Swap Register. Swaps the content of a register with the top of the stack.
	| Swap2Registers Register Register	-- swprr; Swap 2 Registers. Swaps the content of a register with another register.
	| Trap Int				-- trap; Trap to environment function. Trap invokes a systemcall, which one is determined by its argument. Currently just 1 call exists, print the topmost element on the stack as an integer in the output window.
	| Unlink				-- unlink; Free memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
	| ExclusiveOr				-- xor; Exclusive Or. Replaces 2 top stack values with the bitwise exclusive or of those values.
	| StoreHeap				-- sth; Store into Heap. Pops 1 value from the stack and stores it into the heap. Pushes the heap address of that value on the stack.
	| StoreMultipleHeap Int			-- stmh; Store Multiple into Heap. Pops values from the stack and stores it into the heap, retaining the order of the values. Same as single store variant but the inline parameter is size. Pushes the heap address of the last value on the stack.
	| LoadHeap Int				-- ldh; Load from Heap. Pushes a value pointed to by the value at the top of the stack. The pointer value is oﬀset by a constant oﬀset.
	| LoadMultipleHeap Int Int		-- ldmh; Load Multiple from Heap. Pushes values pointed to by the value at the top of the stack. The pointer value is oﬀset by a constant oﬀset. Same as single load variant but the second inline parameter is size. 

$( derive makeIs ''Instruction)

instance Show Instruction where
	show (Label l)				= l ++ ":"
	show (Add)				= "add"
	show (AdjustStack n)			= "ajs " ++ show n
	show (And)				= "and"
	show (BranchAlways l)			= "bra " ++ l
	show (BranchOnFalse l)			= "brf " ++ l
	show (BranchOnTrue l)			= "brt " ++ l
	show (BranchToSubroutine l)		= "bsr " ++ l
	show (Divide)				= "div"
	show (Equal)				= "eq"
	show (GreaterEqual)			= "ge"
	show (GreaterThan)			= "gt"
	show (Halt)				= "halt"
	show (JumpToSubroutine)			= "jsr"
	show (LoadViaAddress n)			= "lda " ++ show n
	show (LoadAddressOfAddress n)		= "ldaa " ++ show n
	show (LoadConstant n)			= "ldc " ++ show n
	show (LoadLocal n)			= "ldl " ++ show n
	show (LoadLocalAddress n)		= "ldla " ++ show n
	show (LoadMultipleViaAddress n m)	= "ldma " ++ show n ++ " " ++ show m
	show (LoadMultipleLocal n m)		= "ldml " ++ show n ++ " " ++ show m
	show (LoadMultipleFromStack n m)	= "ldms " ++ show n ++ " " ++ show m
	show (LoadRegister r)			= "ldr " ++ show r
	show (LoadRegisterFromRegister r1 r2)	= "ldrr " ++ show r1 ++ " " ++ show r2
	show (LoadFromStack n)			= "lds " ++ show n
	show (LoadStackAddress n)		= "ldsa " ++ show n
	show (LesserEqual)			= "le"
	show (Link n)				= "link " ++ show n
	show (LesserThan)			= "lt"
	show (Modulo)				= "mod"
	show (Multiply)				= "mul"
	show (NotEqual)				= "ne"
	show (Negation)				= "neg"
	show (NoOperation)			= "nop"
	show (Not)				= "not"
	show (Or)				= "or"
	show (Return)				= "ret"
	show (StoreViaAddress n)		= "sta " ++ show n
	show (StoreLocal n)			= "stl " ++ show n
	show (StoreMultipleViaAddress n m)	= "stma " ++ show n ++ " " ++ show m
	show (StoreMultipleLocal n m)		= "stml " ++ show n ++ " " ++ show m
	show (StoreMultipleIntoStack n m)	= "stms " ++ show n ++ " " ++ show m
	show (StoreRegister r)			= "str " ++ show r
	show (StoreIntoStack n)			= "sts " ++ show n
	show (Substraction)			= "sub"
	show (SwapValues)			= "swp"
	show (SwapRegister r)			= "swpr " ++ show r
	show (Swap2Registers r1 r2)		= "swprr " ++ show r1 ++ " " ++ show r2
	show (Trap n)				= "trap " ++ show n
	show (Unlink)				= "unlink"
	show (ExclusiveOr)			= "xor"
	show (StoreHeap)			= "sth"
	show (StoreMultipleHeap n)		= "stmh " ++ show n
	show (LoadHeap n)			= "ldh " ++ show n
	show (LoadMultipleHeap n m)		= "ldmh " ++ show n ++ " " ++ show m

type Program = [Instruction]

showProgram :: Program -> String
showProgram [] = ""
showProgram (x:xs) = if isLabel x
	then show x ++ showProgram xs
	else "\t" ++ show x ++ "\n" ++ showProgram xs
