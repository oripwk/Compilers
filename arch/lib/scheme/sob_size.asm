 SOB_SIZE:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);

  MOV(R0, FPARG(0));
  MOV(R1, IND(R0));
  CMP(R1, IMM(T_VOID));
  JUMP_EQ(SOB_SIZE_VOID);
  CMP(R1, IMM(T_NIL));
  JUMP_EQ(SOB_SIZE_NIL); 
  CMP(R1, IMM(T_BOOL));
  JUMP_EQ(SOB_SIZE_BOOL); 
  CMP(R1, IMM(T_CHAR));
  JUMP_EQ(SOB_SIZE_CHAR); 
  CMP(R1, IMM(T_INTEGER));
  JUMP_EQ(SOB_SIZE_INTEGER);
  CMP(R1, IMM(T_STRING));
  JUMP_EQ(SOB_SIZE_STRING);
  CMP(R1, IMM(T_SYMBOL));
  JUMP_EQ(SOB_SIZE_SYMBOL);
  CMP(R1, IMM(T_PAIR));
  JUMP_EQ(SOB_SIZE_PAIR);
  CMP(R1, IMM(T_VECTOR));
  JUMP_EQ(SOB_SIZE_VECTOR);
  CMP(R1, IMM(T_CLOSURE));
  JUMP_EQ(SOB_SIZE_CLOSURE);
  
SOB_SIZE_VOID:
  MOV(R0, 1);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_NIL:
  MOV(R0, 1);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_BOOL:
  MOV(R0, 2);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_CHAR:
  MOV(R0, 2);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_INTEGER:
  MOV(R0, 2);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_STRING:
  MOV(R0, INDD(R0, 1));
  ADD(R0, 2)
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_SYMBOL:
  MOV(R0, 2);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_PAIR:
  MOV(R0, 3);
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_VECTOR:
  MOV(R0, INDD(R0, 1));
  ADD(R0, 2)
  JUMP(EXIT_SOB_SIZE);
  
SOB_SIZE_CLOSURE:
  MOV(R0, 3);
  JUMP(EXIT_SOB_SIZE);
  
EXIT_SOB_SIZE:
  POP(R1);
  POP(FP);
  RETURN;
  