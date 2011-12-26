#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "defs.h"

#define BIT(p) (((i32) 1) << p)

// TODO
i64 INITIAL_BITMAP = 0;

GameState* newGame() {
    GameState* newNode = malloc(sizeof(GameState));
    newNode->turn = 0;

    i32* pieces = &(newNode->pieces);
    for (int i=0; i<NUM_PLAYERS; i++) pieces[i] = 0;
    for (int i=0; i<NUM_PLAYERS; i++) for (int j=0; j<NUM_PIECES; j++) pieces[i] |= BIT(j);

    i64* board = &(newNode->board);
    for (int i=0; i<BOARD_SIZE; i++) board[i] = 0;

// TODO
/*
    newNode->corners[0] = addCorner(0,0,UPPER_LEFT,INITIAL_BITMAP,NULL);
    newNode->corners[1] = addCorner(BOARD_SIZE-1,0,UPPER_RIGHT,INITIAL_BITMAP,NULL);
    newNode->corners[2] = addCorner(BOARD_SIZE-1,BOARD_SIZE-1,LOWER_LEFT,INITIAL_BITMAP,NULL);
    newNode->corners[3] = addCorner(0,BOARD_SIZE-1,LOWER_RIGHT,INITIAL_BITMAP,NULL);
*/

    for (int i=0; i<NUM_PLAYERS; i++) {
        calculateCornerValidNeighborhoods(&(newNode->corners[i]), &(newNode->board), i);
    }


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
    cPieces[player-1] = cPieces[player-1] ^ BIT(piece);
}

void setBoard(GameState* parent, GameState* child, i8 player, Corner* origin, Cell* placement) {
    i64* pBoard = &(parent->board);
    i64* cBoard = &(child->board);
    for (i8 i=0; i<BOARD_SIZE; i++) cBoard[i] = pBoard[i];
    assign(cBoard, player, origin, placement);
}

void setCorners(GameState* parent, GameState* child, i8 player, Corner* origin, Corner* newCorners) {
    Corner** pCorners = &(parent->corners);
    Corner** cCorners = &(child->corners);

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        if (p != player)
            cCorners[p] = getNextCornersForNonMovingPlayer(pCorners[p], &(child->board));
    }

    cCorners[player] = getNextCornersForMovingPlayer(cCorners[player], &(child->board), player, origin, newCorners);
}

GameState* addChild(
    GameState* parent,
    i8 player,
    i8 piece,
    Corner* origin,
    Placement* placement,
    Corner* corners
) {
    GameState* child = malloc(sizeof(GameState));
    child->turn = parent->turn + 1;
    setPieces(parent, child, player, piece);
    setBoard(parent, child, player, origin, placement->cells);
    setCorners(parent, child, player, origin, placement->corners);
    child->parent = parent;
    setHeuristicScores(child);
    return child;
}

// TODO These all need to be depth-specific; also, there's no point in separating corners storage and argument-passing wise from the rest
// very not thread-safe
i8 player;
i32 pieces;
Corner* corners;

i8 piece = NUM_PIECES;
Corner* corner = NULL;
Placement* placement = NULL;

void nextPlacement() {
    assert(pieces & BIT(piece));
    assert(corner != NULL);

    if (placement == NULL)
        placement = placements[piece][corner->corner];

    while ((placement != NULL) && ((placement->bitmap & corner->bitmap) != placement->bitmap))
        placement = placement->next;
}

void nextCorner() {
    assert(placement == NULL);
    assert(pieces & BIT(piece));

    if (corner == NULL)
        corner = corners;
    else
        corner = corner->next;
}

void nextPiece() {
    assert(placement == NULL);
    assert(corner == NULL);

    if (piece == NUM_PIECES)
        piece = 0;

    while (piece < NUM_PIECES && !(pieces & BIT(piece)))
        piece++;
}

void initChildrenSearch(GameState* state) {
    player = (state->turn % 4) + 1;
    pieces = (&(state->pieces))[player-1];

    nextPiece();
    if (piece < NUM_PIECES) {
        calculateCornerValidNeighborhoods(state->corners, &(state->board), player);

        nextCorner();
        if (corner != NULL) {
            nextPlacement();
        }
    }
}

void cleanupChildrenSearch(GameState* state) {
    // TODO
}

GameState* nextChild(GameState* state) {
    if (placement != NULL) {
        GameState* child = addChild(state, player, piece, corner, placement, corners);
        nextPlacement();
        return child;
    }

    // we are out of placements to search at this corner, but not corners, so we try one at the next corner
    if (corner != NULL) {
        nextCorner();
        nextPlacement();
        return nextChild(state);
    }

    // we are out of placements and corners to search for the current piece, so we try the next piece
    if (piece < NUM_PIECES) {
        nextPiece();
        nextCorner();
        nextPlacement();
        return nextChild(state);
    }

    return NULL;
}
