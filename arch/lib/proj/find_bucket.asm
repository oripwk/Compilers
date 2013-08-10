/* proj/find_bucket.asm
 * 
 * Takes a flag, a variable name and the address of the
 * symbol table and returns the variables bucket. If a
 * bucket is not found, then if flag is 1, a new bucket
 * is allocated, added to the symbol table and returned.
 * Otherwise, zero is returned.
 * 
 * Programmer: Ori Popowski, 2013.
 */

FIND_BUCKET:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	MOV(R1, FPARG(1));  //start
	MOV(R2, FPARG(2));  //address of name
	
FIND_BUCKET_LOOP:
	CMP(R1, IMM(0)); // end of list
	JUMP_EQ(FIND_BUCKET_NOT_FOUND);
	MOV(R3, R1);
	CMP(M(mem)[INDD(R1, 1)], R2);
	JUMP_EQ(FIND_BUCKET_EXIT);
	MOV(R1, INDD(R1, 3));
	JUMP(FIND_BUCKET_LOOP);
	
FIND_BUCKET_NOT_FOUND:
	CMP(FPARG(0), 0);
	JUMP_EQ(FIND_BUCKET_EXIT);
	PUSH(IMM(4));
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R3, 3), R0);
	MOV(R1, R0);
	
FIND_BUCKET_EXIT:
	MOV(R0, R1);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;