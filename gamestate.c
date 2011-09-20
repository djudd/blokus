#include <stdlib.h>
#include <stdbool.h>
#include "defs.h"

GameState* newGame() {
    GameState* newNode = malloc(sizeof(GameState));
    newNode->turn = 0;

    i32* pieces = &(newNode->pieces);
    for (int i=0; i<NUM_PLAYERS; i++) pieces[i] = 0;
    for (int i=0; i<NUM_PLAYERS; i++) for (int j=0; j<NUM_PIECES; j++) pieces[i] |= (((i32) 1) << j);

    i64* board = &(newNode->board);
    for (int i=0; i<BOARD_SIZE; i++) board[i] = 0;

    newNode->next = NULL;
    newNode->parent = NULL;

    return newNode;
}

void destroy(GameState* state) {
    if (state == NULL)
        return;
    destroy(state->next);
    free(state);
}

void setPieces(GameState* parent, GameState* child, i8 player, i8 piece) {
    i32* pPieces = &(parent->pieces);
    i32* cPieces = &(child->pieces);
    for (i8 i=0; i<NUM_PLAYERS; i++) cPieces[i] = pPieces[i];
    cPieces[player-1] = cPieces[player-1] ^ (((i32) 1) << piece);
}

void setBoard(GameState* parent, GameState* child, i8 player, Corner* origin, Cell* placement) {
    i64* pBoard = &(parent->board);
    i64* cBoard = &(child->board);
    for (i8 i=0; i<BOARD_SIZE; i++) cBoard[i] = pBoard[i];
    assign(cBoard, player, origin, placement);
}

// TODO: memcpy (or own similar implementation)?
GameState* addChild(GameState* parent, i8 player, i8 piece, Corner* origin, Cell* placement, GameState* next) {
    GameState* child = malloc(sizeof(GameState));
    child->turn = parent->turn + 1;
    setPieces(parent, child, player, piece);
    setBoard(parent, child, player, origin, placement);
    child->parent = parent;
    child->next = next;
    setHeuristicScores(child);
    return child;
}

GameState* children(GameState* state) {
    i8 player = (state->turn % 4) + 1;
    i32 pieces = (&(state->pieces))[player-1];

    Corner* corners = availableCorners(&(state->board), player, state->turn);

    GameState* child = NULL;

    for (i8 piece=0; piece<NUM_PIECES; piece++) {
        if (pieces & (((i32) 1) << piece)) {
            Corner* corner = corners;
            while (corner != NULL) {
                Placement* placement = placements[piece][corner->corner];
                while (placement != NULL) {
                    bool fits = (placement->bitmap & corner->bitmap) == placement->bitmap;
                    if (fits) {
                        child = addChild(state, player, piece, corner, placement->cells, child);
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