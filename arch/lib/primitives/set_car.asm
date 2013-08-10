SET_CAR:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	
	MOV(R0, FPARG(IMM(2)));
	MOV(R1, FPARG(IMM(3)));
	MOV(INDD(R0, 1), R1);
	MOV(R0, IMM(10)) // void
	
	POP(R1);
	POP(FP);
	RETURN;