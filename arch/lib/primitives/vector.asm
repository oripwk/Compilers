VECTOR:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, FPARG(IMM(2)));
	CMP(IND(R0), IMM(T_VECTOR));
	JUMP_NE(VECTOR_NOT_VECTOR);
	MOV(R0, IMM(14));
	JUMP(VECTOR_EXIT);
	VECTOR_NOT_VECTOR:
	MOV(R0, IMM(12));
	VECTOR_EXIT:
	POP(FP);
	RETURN;