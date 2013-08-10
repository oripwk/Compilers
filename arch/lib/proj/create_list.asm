/* proj/create_list.asm

 * Takes an absolute address to argument list in the stack,
 * and number of arguments, and recursively allocates
 * to pairs and returns a list of these arguments.
 * 
 * Programmer: Ori Popowski, 2013.
 */


CREATE_LIST:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  
  MOV(R1, FPARG(0)); // pointer to beginning of elements in the stack
  MOV(R2, FPARG(1)); //size of list
  
  CMP(R2, IMM(0));
  JUMP_EQ(CREATE_LIST_NIL);
  
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  
  MOV(IND(R0), IMM(T_PAIR));
  MOV(INDD(R0, 1), STACK(R1));
  MOV(R3, R0);
  MOV(R4, R0); // head, not touched.
  DECR(R2);
  DECR(R1);
CREATE_LIST_LOOP1:
  CMP(R2, IMM(0));
  JUMP_EQ(CREATE_LIST_LOOP1_EXIT);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), IMM(T_PAIR));
  MOV(INDD(R0, 1), STACK(R1));
  MOV(INDD(R3, 2), R0);
  MOV(R3, R0) // prev
  DECR(R2);
  DECR(R1);
  JUMP(CREATE_LIST_LOOP1);
CREATE_LIST_LOOP1_EXIT:
  MOV(INDD(R3, 2), IMM(11));
  MOV(R0, R4);
  JUMP(CREATE_LIST_EXIT);
CREATE_LIST_NIL:
  MOV(R0, IMM(11)) // null
  JUMP(CREATE_LIST_EXIT);
CREATE_LIST_EXIT:
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
  
  
  
  
  
  
  
  
  
  
  
  