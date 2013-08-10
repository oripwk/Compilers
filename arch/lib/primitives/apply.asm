// APPLY:
	// PUSH(FP);
	// MOV(FP, SP);
	// PUSH(R1);
	// PUSH(R2);
	// PUSH(R3);
	// PUSH(R4);
	// PUSH(R5);
	// PUSH(R6);
	// PUSH(R7);
	// PUSH(R8);
	// PUSH(R9);
	// PUSH(R10);
	// PUSH(R11);
	
	// MOV(R1, FPARG(1)); // m
	// MOV(R2, FPARG(2)); // proc
	
	// MOV(R4, FP);
	// SUB(R4, 6); // points to beginning of args
	// MOV(R3, R1);
	// SUB(R3, 2); // num of non-list args
	// MOV(R11, R3);
// APPLY_LOOP1: // push non-list args
	// CMP(R3, IMM(0));
	// JUMP_EQ(EXIT_APPLY_LOOP1);
	// PUSH(STACK(R4));
	// DECR(R4);
	// DECR(R3)
	// JUMP(APPLY_LOOP1);
// EXIT_APPLY_LOOP1:
	// MOV(R5, STACK(R4)); // R5 points to list
	// MOV(R7, 0); // num of objs in list
// APPLY_PUSH_LIST:
	// CMP(IND(R5), T_NIL);
	// JUMP_EQ(EXIT_APPLY_PUSH_LIST);
	// MOV(R6, INDD(R5, 1)); // car
	// PUSH(R6);
	// INCR(R7);
	// MOV(R5, INDD(R5, 2)); // cdr
	// JUMP(APPLY_PUSH_LIST);
// EXIT_APPLY_PUSH_LIST:
	// ADD(R7, R11); // total number of args
	// MOV(R8, SP); 
	// DECR(R8); // R8 points to last arg
	// MOV(R9, R7); // R9 - total num of args
// APPLY_REVERSE_ARGS:
	// CMP(R9, IMM(0));
	// JUMP_EQ(EXIT_APPLY_REVERSE_ARGS);
	// PUSH(STACK(R8));
	// DECR(R8);
	// DECR(R9);
	// JUMP(APPLY_REVERSE_ARGS);
// EXIT_APPLY_REVERSE_ARGS:
//	now copy down
	// MOV(R10, SP);
	// SUB(R10, R7);
	// MOV(R9, R7);
	// INCR(R8);
// APPLY_LOOP2:
	// CMP(R9, IMM(0));
	// JUMP_EQ(EXIT_APPLY_LOOP2);
	// MOV(STACK(R8), STACK(R10));
	// INCR(R8);
	// INCR(R10);
	// DECR(R9);
	// JUMP(APPLY_LOOP2);
// EXIT_APPLY_LOOP2:
	// DROP(R7);
	// MOV(R15, SP);
	// MOV(R14, R7);
	// PUSH(R7); // num of args for proc
	// PUSH(INDD(R2, 1)); // env
	
	// CALLA(INDD(R2, 2));
	
	// MOV(SP, R15);
	// MOV(R7, R14);
	// DROP(R7);
	
	// POP(R11);
	// POP(R10);
	// POP(R9);
	// POP(R8);
	// POP(R7);
	// POP(R6);
	// POP(R5);
	// POP(R4);
	// POP(R3);
	// POP(R2);
	// POP(R1);
	// POP(FP);
	// RETURN;
APPLY:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);
	PUSH(R6);
	
	MOV(R1, FPARG(3)); // args list
	MOV(R2, IMM(0)); // counter
APPLY_LOOP1:
	CMP(IND(R1), T_NIL);
	JUMP_EQ(APPLY_EXIT_LOOP1);
	PUSH(INDD(R1, 1));
	MOV(R1, INDD(R1, 2));
	INCR(R2);
	JUMP(APPLY_LOOP1);
APPLY_EXIT_LOOP1:
	MOV(R3, SP);
	SUB(R3, R2); // first arg
	MOV(R4, SP);
	DECR(R4);	// last arg
APPLY_REVERSE_LOOP:
	CMP(R3, R4);
	JUMP_GE(APPLY_EXIT_REVERSE);
	// swap
	MOV(R5, STACK(R3));
	MOV(STACK(R3), STACK(R4));
	MOV(STACK(R4), R5);
	INCR(R3);
	DECR(R4);
	JUMP(APPLY_REVERSE_LOOP);
APPLY_EXIT_REVERSE:
	PUSH(R2); // args num
	MOV(R6, FPARG(2)); // closure
	PUSH(INDD(R6, 1)); // env
	CALLA(INDD(R6, 2));
	MOV(R1, STARG(0)); // number of params in the stack
    ADD(R1, IMM(2)); // size of frame
    SUB(SP, R1);

	
	POP(R6);
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
	
	
	