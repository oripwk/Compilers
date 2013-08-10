STRING_REF:
   PUSH(FP);
   MOV(FP, SP);
   PUSH(R1);
   PUSH(R2);
   PUSH(R4);
   
   MOV(R1, FPARG(IMM(2))); // string
   MOV(R2, FPARG(IMM(3))); 
   
   MOV(R2, INDD(R2, 1)); // index
   ADD(R1, IMM(2)); // points to beginning of string
   
   MOV(R4, IMM(0));
STRING_REF_LOOP:
   CMP(R4, R2);
   JUMP_EQ(STRING_REF_EXIT);
   INCR(R4);
   JUMP(STRING_REF_LOOP);
STRING_REF_EXIT:
   PUSH(INDD(R1, R4));
   CALL(MAKE_SOB_CHAR)
   DROP(1);
   
   POP(R4);
   POP(R2);
   POP(R1);
   POP(FP);
   RETURN;