BIN_DIV:
   PUSH(FP);
   MOV(FP, SP);
   PUSH(R1);
   
   MOV(R0, FPARG(IMM(2)));
   MOV(R1, FPARG(IMM(3)));
   MOV(R0, INDD(R0, 1));
   MOV(R1, INDD(R1, 1));
   DIV(R0, R1);
   PUSH(R0);
   CALL(MAKE_SOB_INTEGER)
   DROP(1);
   
   POP(R1);
   POP(FP);
   RETURN;