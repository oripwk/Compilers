BOOLEAN:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, FPARG(IMM(2)));
	CMP(IND(R0), IMM(T_BOOL));
	JUMP_NE(BOOLEAN_NOT_BOOLEAN);
	MOV(R0, IMM(14));
	JUMP(BOOLEAN_EXIT);
	BOOLEAN_NOT_BOOLEAN:
	MOV(R0, IMM(12));
	BOOLEAN_EXIT:
	POP(FP);
	RETURN;