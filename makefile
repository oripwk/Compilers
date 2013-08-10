all: out

out : ./arch/out.o
	gcc -m32 -o out ./arch/out.o

./arch/out.o : out.c
	gcc -c -m32 -o ./arch/out.o ./arch/out.c

clean : 
	rm -f arch/*.o
	rm -f out
