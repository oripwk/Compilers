STRING_SET:
   PUSH(FP);
   MOV(FP, SP);
   PUSH(R1);
   PUSH(R2);
   PUSH(R3);
   
   MOV(R3, FPARG(4)); //char
   MOV(R2, FPARG(3)); //index
   MOV(R1, FPARG(2)); //string
   
   MOV(R2, INDD(R2, 1));
   ADD(R2, IMM(2));
   MOV(INDD(R1, R2), INDD(R3, 1));
   MOV(R0, IMM(10));
   
   POP(R3);
   POP(R2);
   POP(R1);
   POP(FP);
   RETURN;