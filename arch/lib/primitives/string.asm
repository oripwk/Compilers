STRING:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, FPARG(IMM(2)));
	CMP(IND(R0), IMM(T_STRING));
	JUMP_NE(STRING_NOT_STRING);
	MOV(R0, IMM(14));
	JUMP(STRING_EXIT);
	STRING_NOT_STRING:
	MOV(R0, IMM(12));
	STRING_EXIT:
	POP(FP);
	RETURN;