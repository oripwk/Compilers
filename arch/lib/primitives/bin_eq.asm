BIN_EQ:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	
	MOV(R0, FPARG(IMM(2)));
	MOV(R1, FPARG(IMM(3)));
	MOV(R0, INDD(R0, 1));
	MOV(R1, INDD(R1, 1));
	CMP(R0, R1);
	JUMP_EQ(BIN_EQ_TRUE);
	MOV(R0, IMM(12)) // false
	JUMP(BIN_EQ_EXIT);
	BIN_EQ_TRUE:
	MOV(R0, IMM(14)) // true
	BIN_EQ_EXIT:
	
	POP(R1);
	POP(FP);
	RETURN;