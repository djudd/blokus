objects = main.o board.o cell.o
cflags = -c -std=c99

blokus : ${objects}
	gcc -o blokus ${objects}

main.o : main.c defs.h
	gcc ${cflags} main.c
board.o : board.c defs.h
	gcc ${cflags} board.c
cell.o : board.c defs.h
	gcc ${cflags} cell.c
clean :
	rm -f blokus ${objects}
