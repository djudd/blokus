#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
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
        case 10: return 5*5*5*5*5*5*5*5*5*5;
        case 11: return 5*5*5*5*5*5*5*5*5*5*5;
        case 12: return 5*5*5*5*5*5*5*5*5*5*5*5;
        case 13: return 5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 14: return 5*5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 15: return 5*5*5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 16: return 5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 17: return 5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 18: return 5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5*5;
        case 19: return 5*5*5*5**5*5*5*5*55*5*5*5*5*5*5*5*5*5;
    }
}

void setOwner(i64* board, i8 player, i8 x, i8 y) {
//    i64 old = board[x];
    board[x] = board[x] + (pow5(y) * player);
//    printf("setting (%d, %d) owner to %d; row was %llu, now %llu\n", x, y, player, old, board[x]);
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

Corner* availableCorners(i64* board, i8 player, i8 turn) {
    switch (turn) {
        case 0: return addCorner(0,0,UPPER_LEFT,NULL);
        case 1: return addCorner(BOARD_SIZE-1,0,UPPER_RIGHT,NULL);
        case 2: return addCorner(BOARD_SIZE-1,BOARD_SIZE-1,LOWER_LEFT,NULL);
        case 3: return addCorner(0,BOARD_SIZE-1,LOWER_RIGHT,NULL);
        default:
            ; // http://old.nabble.com/-Bug-c-37231---New:-GCC-does-not-compile-code-with-label-statements-that-are-followed-by-a-declaration-td19140837.html
            Corner* cell = NULL;
            for (i8 x=0; x<BOARD_SIZE; x++) {
                for (i8 y=0; y<BOARD_SIZE; y++) {
                    if (!hasAnyOwner(board, x, y) && !touchesSide(board, player, x, y)) {
                        signed char corner = touchesCorner(board, player, x, y);
                        if (corner >= 0) {
                            cell = addCorner(x, y, corner, cell);
                        }
                    }
                }
            }
            return cell;
    }
}

// TODO optimize - better memcpy implementation? Combine malloc with GameState's?
i64* afterMove(i64* board, i8 player, Corner* origin, Cell* placement) {
    i64* child = (i64*) malloc(BOARD_SIZE * sizeof(i64));
    for (i8 i=0; i<BOARD_SIZE; i++)
        child[i] = board[i];
//    memcpy(child, board, BOARD_SIZE);
    assign(child, player, origin, placement);
    return child;
}

i64* empty() {
    i64* board = (i64*) malloc(BOARD_SIZE * sizeof(i64));
    for (i8 i=0; i<BOARD_SIZE; i++)
        board[i] = 0;
    return board;
}

GameState* addChild(GameState* parent, i8 player, i8 piece, Corner* origin, Cell* placement, GameState* next) {
    GameState* newNode = malloc(sizeof(GameState));

    newNode->turn = parent->turn + 1;

    // TODO cleanup and optimize - separate function, share malloc (one per new GameState)?
    i32* pieces = malloc(4*sizeof(i32));
    for (int i=0; i<4; i++) pieces[i] = parent->pieces[i];
    pieces[player-1] = pieces[player-1] ^ (1 << piece);
    newNode->pieces = pieces;

    newNode->board = afterMove(parent->board, player, origin, placement);

    newNode->next = next;

    return newNode;
}

GameState* children(GameState* state) {
    i8 player = (state->turn % 4) + 1;
    i32 pieces = state->pieces[player-1];

    Corner* corners = availableCorners(state->board, player, state->turn);

    GameState* child = NULL;

    for (i8 piece=0; piece<NUM_PIECES; piece++) {
        if (pieces & (1 << piece)) {
            Corner* corner = corners;
            while (corner != NULL) {
                Placement* placement = placements[piece][corner->corner];
                while (placement != NULL) {
                    if ((placement->bitmap & corner->bitmap) == placement->bitmap) {
                        addChild(state, player, piece, corner, placement->cells, child);
                    }
                    placement = placement->next;
                }
                corner = corner->next;
            }
        }
    }

    destroyCorners(corners);

    return child;
}
