objects = main.o board.o cell.o placement.o piece.o
#cc = gcc
cc = clang
cflags = -c -std=c99 -ggdb -Wall -O0
#cflags = -c -std=c99 -Wall -O3

blokus : ${objects}
	${cc} -o blokus ${objects}

piece.o: piece.c defs.h
	${cc} ${cflags} piece.c
placement.o : placement.c defs.h
	${cc} ${cflags} placement.c
main.o : main.c defs.h
	${cc} ${cflags} main.c
board.o : board.c defs.h
	${cc} ${cflags} board.c
cell.o : board.c defs.h
	${cc} ${cflags} cell.c
clean :
	rm -f blokus ${objects}
