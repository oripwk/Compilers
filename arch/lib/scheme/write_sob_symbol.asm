/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Ori Popowski, 2013.
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  MOV(R0, INDD(R0, 1));
  MOV(R1, INDD(R0, 1));
  MOV(R2, R0);
  ADD(R2, IMM(2));
 SYMBOL_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(SYMBOL_LOOP_EXIT);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  INCR(R2);
  DECR(R1);
  JUMP(SYMBOL_LOOP);
SYMBOL_LOOP_EXIT:
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

