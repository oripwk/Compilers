INTEGER_CHAR:   
   PUSH(FP);
   MOV(FP, SP);
   MOV(R0, FPARG(IMM(2)));
   MOV(R0, INDD(R0, 1));
   PUSH(R0);
   CALL(MAKE_SOB_CHAR)
   DROP(1);
   POP(FP);
   RETURN;