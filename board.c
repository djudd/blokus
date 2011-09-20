#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "defs.h"

i64 pow5(i8 n) {
    switch (n) {
        case 0: return 1;
        case 1: return 5;
        case 2: return 5*5;
        case 3: return 5*5*5;
        case 4: return 5*5*5*5;
        case 5: return 5*5*5*5*5;
        case 6: return 5*5*5*5*5*5;
        case 7: return 5*5*5*5*5*5*5;
        case 8: return 5*5*5*5*5*5*5*5;
        case 9: return 5*5*5*5*5*5*5*5*5;
        case 10: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 11: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 12: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 13: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 14: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 15: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 16: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 17: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 18: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        case 19: return 5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu*5llu;
        default: assert(0); return -1;
    }
}

void setOwner(i64* board, i8 player, i8 x, i8 y) {
    assert(x >= 0 && x < BOARD_SIZE);
    assert(y >= 0 && y < BOARD_SIZE);
    board[x] = board[x] + (pow5(y) * player);
}

i8 owner(i64* board, i8 x, i8 y) {
    return (board[x] / pow5(y)) % 5;
}

bool hasOwner(i64* board, i8 player, i8 x, i8 y) {
    return (board[x] / pow5(y)) % 5 == player;
}

bool hasAnyOwner(i64* board, i8 x, i8 y) {
    return (board[x] / pow5(y)) % 5 > 0;
}

void assign(i64* board, i8 player, Corner* origin, Cell* cell) {
    setOwner(board, player, origin->x, origin->y);
    while (cell != NULL) {
        setOwner(board, player, cell->x + origin->x, cell->y + origin->y);
        cell = cell->next;
    }
}

i8 touchesCorner(i64* board, i8 player, i8 x, i8 y) {
    if (x+1 < BOARD_SIZE) {
        if (y+1 < BOARD_SIZE && hasOwner(board, player, x+1, y+1)) {
            return LOWER_RIGHT;
        }
        if (y-1 >= 0 && hasOwner(board, player, x+1, y-1)) {
            return UPPER_RIGHT;
        }
    }
    if (x-1 >= 0) {
        if (y+1 < BOARD_SIZE && hasOwner(board, player, x-1, y+1)) {
            return LOWER_LEFT;
        }
        if (y-1 >= 0 && hasOwner(board, player, x-1, y-1)) {
            return UPPER_LEFT;
        }
    }

    return -1;
}

bool touchesSide(i64* board, i8 player, i8 x, i8 y) {
    if (x+1 < BOARD_SIZE && hasOwner(board, player, x+1, y))
        return true;
    if (x-1 >= 0 && hasOwner(board, player, x-1, y))
        return true;
    if (y+1 < BOARD_SIZE && hasOwner(board, player, x, y+1))
        return true;
    if (y-1 >= 0 && hasOwner(board, player, x, y-1))
        return true;

    return false;
}

bool onBoard(i8 x, i8 y) {
    return x >= 0 && x < BOARD_SIZE && y >= 0 && y < BOARD_SIZE;
}

bool valid(i64* board, i8 player, i8 x, i8 y) {
    return onBoard(x, y) && !hasAnyOwner(board, x, y) && !touchesSide(board, player, x, y);
}

#define ADD_BIT_UL(i,j) if (valid(board, player, x+(i), y+(j))) result |= bit((i),(j));
#define ADD_BIT_UR(i,j) if (valid(board, player, x-(i), y+(j))) result |= bit(-(i),(j));
#define ADD_BIT_LL(i,j) if (valid(board, player, x+(i), y-(j))) result |= bit((i),-(j));
#define ADD_BIT_LR(i,j) if (valid(board, player, x-(i), y-(j))) result |= bit(-(i),-(j));

i64 calcBitmap(i8 x, i8 y, i8 corner, i64* board, i8 player) {
    i64 result = 0;
    switch (corner) {
        case UPPER_LEFT:
            ;
            ADD_BIT_UL(1,-3);
            ADD_BIT_UL(0,-2);
            ADD_BIT_UL(1,-2);
            ADD_BIT_UL(2,-2);
            ADD_BIT_UL(1,-1);
            ADD_BIT_UL(2,-1);
            ADD_BIT_UL(3,-1);
            ADD_BIT_UL(-2,0);
            ADD_BIT_UL(1,0);
            ADD_BIT_UL(2,0);
            ADD_BIT_UL(3,0);
            ADD_BIT_UL(4,0);
            ADD_BIT_UL(-3,1);
            ADD_BIT_UL(-2,1);
            ADD_BIT_UL(-1,1);
            ADD_BIT_UL(0,1);
            ADD_BIT_UL(1,1);
            ADD_BIT_UL(2,1);
            ADD_BIT_UL(3,1);
            ADD_BIT_UL(-2,2);
            ADD_BIT_UL(-1,2);
            ADD_BIT_UL(0,2);
            ADD_BIT_UL(1,2);
            ADD_BIT_UL(2,2);
            ADD_BIT_UL(-1,3);
            ADD_BIT_UL(0,3);
            ADD_BIT_UL(1,3);
            ADD_BIT_UL(0,4);
            return result;
        case UPPER_RIGHT:
            ;
            ADD_BIT_UR(1,-3);
            ADD_BIT_UR(0,-2);
            ADD_BIT_UR(1,-2);
            ADD_BIT_UR(2,-2);
            ADD_BIT_UR(1,-1);
            ADD_BIT_UR(2,-1);
            ADD_BIT_UR(3,-1);
            ADD_BIT_UR(-2,0);
            ADD_BIT_UR(1,0);
            ADD_BIT_UR(2,0);
            ADD_BIT_UR(3,0);
            ADD_BIT_UR(4,0);
            ADD_BIT_UR(-3,1);
            ADD_BIT_UR(-2,1);
            ADD_BIT_UR(-1,1);
            ADD_BIT_UR(0,1);
            ADD_BIT_UR(1,1);
            ADD_BIT_UR(2,1);
            ADD_BIT_UR(3,1);
            ADD_BIT_UR(-2,2);
            ADD_BIT_UR(-1,2);
            ADD_BIT_UR(0,2);
            ADD_BIT_UR(1,2);
            ADD_BIT_UR(2,2);
            ADD_BIT_UR(-1,3);
            ADD_BIT_UR(0,3);
            ADD_BIT_UR(1,3);
            ADD_BIT_UR(0,4);
            return result;
        case LOWER_LEFT:
            ;
            ADD_BIT_LL(1,-3);
            ADD_BIT_LL(0,-2);
            ADD_BIT_LL(1,-2);
            ADD_BIT_LL(2,-2);
            ADD_BIT_LL(1,-1);
            ADD_BIT_LL(2,-1);
            ADD_BIT_LL(3,-1);
            ADD_BIT_LL(-2,0);
            ADD_BIT_LL(1,0);
            ADD_BIT_LL(2,0);
            ADD_BIT_LL(3,0);
            ADD_BIT_LL(4,0);
            ADD_BIT_LL(-3,1);
            ADD_BIT_LL(-2,1);
            ADD_BIT_LL(-1,1);
            ADD_BIT_LL(0,1);
            ADD_BIT_LL(1,1);
            ADD_BIT_LL(2,1);
            ADD_BIT_LL(3,1);
            ADD_BIT_LL(-2,2);
            ADD_BIT_LL(-1,2);
            ADD_BIT_LL(0,2);
            ADD_BIT_LL(1,2);
            ADD_BIT_LL(2,2);
            ADD_BIT_LL(-1,3);
            ADD_BIT_LL(0,3);
            ADD_BIT_LL(1,3);
            ADD_BIT_LL(0,4);
            return result;
        case LOWER_RIGHT:
            ;
            ADD_BIT_LR(1,-3);
            ADD_BIT_LR(0,-2);
            ADD_BIT_LR(1,-2);
            ADD_BIT_LR(2,-2);
            ADD_BIT_LR(1,-1);
            ADD_BIT_LR(2,-1);
            ADD_BIT_LR(3,-1);
            ADD_BIT_LR(-2,0);
            ADD_BIT_LR(1,0);
            ADD_BIT_LR(2,0);
            ADD_BIT_LR(3,0);
            ADD_BIT_LR(4,0);
            ADD_BIT_LR(-3,1);
            ADD_BIT_LR(-2,1);
            ADD_BIT_LR(-1,1);
            ADD_BIT_LR(0,1);
            ADD_BIT_LR(1,1);
            ADD_BIT_LR(2,1);
            ADD_BIT_LR(3,1);
            ADD_BIT_LR(-2,2);
            ADD_BIT_LR(-1,2);
            ADD_BIT_LR(0,2);
            ADD_BIT_LR(1,2);
            ADD_BIT_LR(2,2);
            ADD_BIT_LR(-1,3);
            ADD_BIT_LR(0,3);
            ADD_BIT_LR(1,3);
            ADD_BIT_LR(0,4);
            return result;
        default: return -1; // should never happen
    }
}

Corner* _addCorner(i8 x, i8 y, i8 corner, i64* board, i8 player, Corner* next) {
    i64 bitmap = calcBitmap(x, y, corner, board, player);
    return addCorner(x, y, corner, bitmap, next);
}

Corner* availableCorners(i64* board, i8 player, i8 turn) {
    switch (turn) {
        case 0: return _addCorner(0,0,UPPER_LEFT,board,player,NULL);
        case 1: return _addCorner(BOARD_SIZE-1,0,UPPER_RIGHT,board,player,NULL);
        case 2: return _addCorner(BOARD_SIZE-1,BOARD_SIZE-1,LOWER_LEFT,board,player,NULL);
        case 3: return _addCorner(0,BOARD_SIZE-1,LOWER_RIGHT,board,player,NULL);
        default:
            ; // http://old.nabble.com/-Bug-c-37231---New:-GCC-does-not-compile-code-with-label-statements-that-are-followed-by-a-declaration-td19140837.html
            Corner* cell = NULL;
            for (i8 x=0; x<BOARD_SIZE; x++) {
                for (i8 y=0; y<BOARD_SIZE; y++) {
                    if (valid(board, player, x, y)) {
                        i8 corner = touchesCorner(board, player, x, y);
                        if (corner >= 0) {
                            cell = _addCorner(x, y, corner, board, player, cell);
                        }
                    }
                }
            }
            return cell;
    }
}

char* toString(i64* board) {
    char* result = malloc(BOARD_SIZE*BOARD_SIZE*2+1);
    int idx = 0;
    for (int i=0; i<BOARD_SIZE; i++) {
        for (int j=0; j<BOARD_SIZE; j++) {
            i8 cellOwner = owner(board, i, j);
            if (cellOwner == 0)
                result[idx++] = '.';
            else
                result[idx++] = (char) cellOwner;

            if (j < BOARD_SIZE-1)
                result[idx++] = ' ';
            else
                result[idx++] = '\n';
        }
    }
    result[idx++] = '\0';
    return result;
}