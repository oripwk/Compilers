/* 
 * Find the SOB in the specified index in the
 * vector, duplicate it and return its clone.
 */

VECTOR_REF:
   PUSH(FP);
   MOV(FP, SP);
   PUSH(R1);
   PUSH(R2);
   PUSH(R3);
   PUSH(R4);
   
   MOV(R1, FPARG(IMM(2))); // vector
   MOV(R2, FPARG(IMM(3))); 
   MOV(R2, INDD(R2, 1)); // index
   MOV(R3, R1);
   ADD(R3, IMM(2)); // beginning of vector
   MOV(R4, IMM(0));
   
VECTOR_REF_LOOP:
   CMP(R4, R2);
   JUMP_EQ(VECTOR_REF_LOOP_EXIT);
   INCR(R4);
   JUMP(VECTOR_REF_LOOP);
   
VECTOR_REF_LOOP_EXIT:
   MOV(R3, INDD(R3, R4)); // SOB at desired index
   PUSH(R1);
   CALL(SOB_SIZE);
   DROP(1);
   MOV(R2, R0); // R2 is size
   PUSH(R0);
   CALL(MALLOC); // R0 points to duplicate
   DROP(1);  
   
VECTOR_REF_COPY:
   MOV(INDD(R0, R2), INDD(R3, R2))
   CMP(R2, IMM(0));
   JUMP_EQ(VECTOR_REF_EXIT);
   DECR(R2);
   JUMP(VECTOR_REF_COPY);
   
VECTOR_REF_EXIT:
   POP(R4);
   POP(R3);
   POP(R2);
   POP(R1);
   POP(FP);
   RETURN;