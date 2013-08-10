/*
- find address of string in constant table
- go through symbol table to find a bucket which points to the address.
- if a bucket is found
	- go through memory to find a T_SYM shich point to that bucket and return it.
- else
	- link a new bucket which point to the string
	- create a T_SYM and point it to the bucket and return it.
*/

/* STRING_SYMBOL:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	
	MOV(R1, FPARG(2)); // address of string
	MOV(R2, M(mem)[STAB_START]); // start of buckets
STRING_SYMBOL_LOOP1:
	CMP(R2, IMM(0));
	JUMP_EQ(STRING_SYMBOL_NOT_FOUND);
	CMP(INDD(R2, 1), R1);
	JUMP_EQ(STRING_SYMBOL_FOUND);
	MOV(R3, R2); // prev
	MOV(R2, INDD(R2, 3));
		
	JUMP(STRING_SYMBOL_LOOP1);
STRING_SYMBOL_NOT_FOUND:
	PUSH(IMM(4));
	CALL(MALLOC);
	DROP(1);
	
	MOV(IND(R0), R0); // tell him his address
	MOV(INDD(R0, 1), R1); // link to the string
	MOV(INDD(R0, 4), IMM(0)); // mark end of list
	MOV(INDD(R3, 4), R0); // link to the table

	MOV(R4, R0);
	
	PUSH(IMM(2));
	CALL(MALLOC);  // create a symbol object
	DROP(1);
	
	MOV(IND(R0), IMM(T_SYMBOL)); // add tag
	MOV(INDD(R0, 1), R4); // point him to the new bucket.
	JUMP(STRING_SYMBOL_EXIT);
STRING_SYMBOL_FOUND:
	// R2 has the address of the bucket
	MOV(R0, IMM(10));
STRING_SYMBOL_LOOP2:
	CMP(IND(R0), IMM(T_SYMBOL));
	JUMP_NE(STRING_SYMBOL_CONTINUE);
	CMP(INDD(R0, 1), R2);
	JUMP_EQ(STRING_SYMBOL_EXIT);
STRING_SYMBOL_CONTINUE:
	INCR(R0);
	JUMP(STRING_SYMBOL_LOOP2);
STRING_SYMBOL_EXIT:
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN; */
	
	

/*
- find address of string in constant table
- go through symbol table to find a bucket which points to the address.
- if a bucket is found
	- go through memory to find a T_SYM shich point to that bucket and return it.
- else
	- link a new bucket which point to the string
	- create a T_SYM and point it to the bucket and return it.
*/

STRING_SYMBOL:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	PUSH(R5);
	PUSH(R7);
	
	MOV(R1, FPARG(2)); // address of string
	MOV(R2, M(mem)[STAB_START]); // start of buckets
	CMP(R1, IND(STAB_START));
	JUMP_LE(STRING_SYMBOL_LOOP1);

// search address of identical string in constant table:
	MOV(R3, IND(CTAB_START)); // start of constant table
	MOV(R5, IND(STAB_START));  // end of constant table
	MOV(R4, INDD(R1, 1));
	ADD(R4, IMM(2)); // size of string sob

STRING_SYMBOL_LOOP3:
	// compare strings
		MOV(R7, IMM(0)); 	
	STRING_SYMBOL_INNER_LOOP1:
		CMP(R7, R4);
		JUMP_EQ(STRING_SYMBOL_FOUND_IN_CONSTAB);
		CMP(INDD(R1, R7), IND(R3));
		JUMP_NE(STRING_SYMBOL_EXIT_INNER_LOOP1);
		INCR(R3);
		INCR(R7);
		JUMP(STRING_SYMBOL_INNER_LOOP1);
STRING_SYMBOL_EXIT_INNER_LOOP1:
	CMP(R3, R5);
	JUMP_GE(STRING_SYMBOL_NOT_IN_CONSTAB); // exhausted constant table, not found.
	INCR(R3);
	JUMP(STRING_SYMBOL_LOOP3);
	
	
STRING_SYMBOL_FOUND_IN_CONSTAB:
	SUB(R3, R4);
	MOV(R1, R3);
	JUMP(STRING_SYMBOL_LOOP1);

STRING_SYMBOL_NOT_IN_CONSTAB:	
STRING_SYMBOL_LOOP1:
	CMP(R2, IMM(0));
	JUMP_EQ(STRING_SYMBOL_NOT_FOUND);
	CMP(INDD(R2, 1), R1);
	JUMP_EQ(STRING_SYMBOL_FOUND);
	MOV(R3, R2); // prev
	MOV(R2, INDD(R2, 3));
		
	JUMP(STRING_SYMBOL_LOOP1);
STRING_SYMBOL_NOT_FOUND:
	PUSH(IMM(4));
	CALL(MALLOC);
	DROP(1);
	
	MOV(IND(R0), R0); // tell him his address
	MOV(INDD(R0, 1), R1); // link to the string
	MOV(INDD(R0, 4), IMM(0)); // mark end of list
	MOV(INDD(R3, 4), R0); // link to the table

	MOV(R4, R0);
	
	PUSH(IMM(2));
	CALL(MALLOC);  // create a symbol object
	DROP(1);
	
	MOV(IND(R0), IMM(T_SYMBOL)); // add tag
	MOV(INDD(R0, 1), R4); // point him to the new bucket.
	JUMP(STRING_SYMBOL_EXIT);
STRING_SYMBOL_FOUND:
	// R2 has the address of the bucket
	MOV(R0, IMM(10));
STRING_SYMBOL_LOOP2:
	CMP(IND(R0), IMM(T_SYMBOL));
	JUMP_NE(STRING_SYMBOL_CONTINUE);
	CMP(INDD(R0, 1), R2);
	JUMP_EQ(STRING_SYMBOL_EXIT);
STRING_SYMBOL_CONTINUE:
	INCR(R0);
	JUMP(STRING_SYMBOL_LOOP2);
STRING_SYMBOL_EXIT:
	POP(R7);
	POP(R5);
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
	