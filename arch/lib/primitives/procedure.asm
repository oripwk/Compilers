PROCEDURE:
	PUSH(FP);
	MOV(FP, SP);
	MOV(R0, FPARG(IMM(2)));
	CMP(IND(R0), IMM(T_CLOSURE));
	JUMP_NE(PROCEDURE_NOT_PROCEDURE);
	MOV(R0, IMM(14));
	JUMP(PROCEDURE_EXIT);
	PROCEDURE_NOT_PROCEDURE:
	MOV(R0, IMM(12));
	PROCEDURE_EXIT:
	POP(FP);
	RETURN;