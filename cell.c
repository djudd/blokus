#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"

Cell* addCell(i8 x, i8 y, Cell* next) {
    Cell* newNode = malloc(sizeof(Cell));
    newNode->x = x;
    newNode->y = y;
    newNode->next = next;
    return newNode;
}

// definitely not thread-safe
Corner* corners;
Corner* nextCorner;

void initCornerStorage() {
    corners = malloc(BOARD_SIZE*BOARD_SIZE*sizeof(Corner));
    nextCorner = corners;
}

Corner* newCorner() {
    Corner* thisCorner = nextCorner;
    nextCorner += sizeof(Corner);
    return thisCorner;
}

void destroyCorners() {
    nextCorner = corners;
}

Corner* addCorner(i8 x, i8 y, i8 corner, i64 bitmap, Corner* next) {
    Corner* newNode = newCorner();
    newNode->x = x;
    newNode->y = y;
    newNode->corner = corner;
    newNode->bitmap = bitmap;
    newNode->next = next;
    return newNode;
}