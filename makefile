objects = main.o board.o cell.o placement.o piece.o gamestate.o heuristics.o search.o

#cc = gcc
cc = clang

#cflags = -c -std=c99 -ggdb -Wall -O0 # debugging
#cflags = -c -std=c99 -g -pg -Wall -O3 -DNDEBUG # profiling
cflags = -c -std=c99 -Wall -O3 -DNDEBUG # production

oflags = -o
#oflags = -o -g -pg # profiling

blokus : ${objects}
	${cc} ${oflags} blokus ${objects}

piece.o: piece.c defs.h
	${cc} ${cflags} piece.c
cell.o : board.c defs.h
	${cc} ${cflags} cell.c
placement.o : placement.c defs.h
	${cc} ${cflags} placement.c
main.o : main.c defs.h
	${cc} ${cflags} main.c
board.o : board.c defs.h
	${cc} ${cflags} board.c
gamestate.o : gamestate.c defs.h
	${cc} ${cflags} gamestate.c
heuristics.o : heuristics.c defs.h
	${cc} ${cflags} heuristics.c
search.o : search.c defs.h
	${cc} ${cflags} search.c
clean :
	rm -f blokus ${objects}
