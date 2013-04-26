module SSM where

data Instruction = Add				-- add; Addition. Replaces 2 top stack values with the addition of those values.
	| AdjustStack Int			-- ajs; Adjust Stack. Adjusts the stackpointer with fixed amount.
	| And					-- and; And. Replaces 2 top stack values with the bitwise and of those values.
	-- | Annote Int Int Int Color String
	| BranchAlways Int			-- bra; Branch Always. Jumps to the destination. Replaces the PC with the destination address.
	| BranchOnFalse Int			-- brf; Branch on False. If a False value is on top of the stack, jump to the destination.
	| BranchOnTrue Int			-- brt; Branch on True. If a True value is on top of the stack, jump to the destination.
	| BranchToSubroutine Int		-- bsr; Branch to subroutine. Pushes the PC on the stack and jumps to the subroutine.
	| Divide				-- div; Division. Replaces 2 top stack values with the division of those values.
	| Equal					-- eq; Test for equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with beq.
	| GreaterEqual				-- ge; Test for greater or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bge.
	| GreaterThan				-- gt; Test for greater then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bgt.
	| Halt					-- halt; Halt execution. Machine stops executing instructions.
	| JumpToSubroutine			-- jsr; Jump to subroutine. Pops a destination from the stack, pushes the PC on the stack and jumps to the destination.
	| LoadViaAddress Int			-- lda; Load via Address. Dereferencing. Pushes the value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
	| LoadAddressOfAddress Int		-- ldaa; Load Address of Address. Pushes the address of a value relative to the address on top of the stack. This instruction effectively adds a constant to the top of the stack.
	| LoadConstant Int			-- lcd; Load Constant. Pushes the inline constant on the stack.
	| LoadLocal Int				-- ldl; Load Local. Pushes a value relative to the markpointer.
	| LoadLocalAddress Int			-- ldla; Load Local Address. Pushes the address of a value relative to the markpointer.
	| LoadMultipleViaAddress Int Int	-- ldma; Load Multiple via Address. Pushes values relative to by the value at the top of the stack. Same as single load variant but second inline parameter is size.
	| LoadMultipleLocal Int Int		-- ldml; Load Multiple Local. Pushes values relative to the markpointer. Same as single load variant but second inline parameter is size.
	| LoadMultipleFromStack Int Int		-- ldms; Load Multiple from Stack. Pushes values relative to the top of the stack. Same as single load variant but second inline parameter is size.
	| LoadRegister Int			-- ldr; Load Register. Pushes a value from a register. Registers 0, 1, 2 and 3 are called PC (program counter), SP (stack pointer), MP (mark pointer) and RR (return register) respectively.
	| LoadRegisterFromRegister Int Int	-- ldrr; Load Register from Register. Copy the content of the second register to the first. Does not affect the stack.
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
	| StoreRegister Int			-- str; Store Register. Pops a value from the stack and stores it in a location relative to the markpointer. See also ldr.
	| StoreIntoStack Int			-- sts; Store into Stack. Pops a value from the stack and stores it in a location relative to the top of the stack.
	| Substraction				-- sub; Substraction. Replaces 2 top stack values with the subtraction of those values.
	| SwapValues				-- swp; Swap values. Swaps the 2 topmost values on the stack.
	| SwapRegister Int			-- swpr; Swap Register. Swaps the content of a register with the top of the stack.
	| Swap2Registers Int Int		-- swprr; Swap 2 Registers. Swaps the content of a register with another register.
	| Trap Int				-- trap; Trap to environment function. Trap invokes a systemcall, which one is determined by its argument. Currently just 1 call exists, print the topmost element on the stack as an integer in the output window.
	| Unlink				-- unlinks; Free memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
	| ExclusiveOr				-- xor; Exclusive Or. Replaces 2 top stack values with the bitwise exclusive or of those values.
