objects = main.o board.o cell.o placement.o
cflags = -c -std=c99 -ggdb

blokus : ${objects}
	gcc -o blokus ${objects}

placement.o : placement.c defs.h
	gcc ${cflags} placement.c
main.o : main.c defs.h
	gcc ${cflags} main.c
board.o : board.c defs.h
	gcc ${cflags} board.c
cell.o : board.c defs.h
	gcc ${cflags} cell.c
clean :
	rm -f blokus ${objects}
