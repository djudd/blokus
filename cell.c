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

void destroyCells(Cell* node) {
    Cell* next = node->next;
    free(node);
    while(next != NULL) {
       node = next;
       next = node->next;
       free(node);
    }
}

Corner* addCorner(i8 x, i8 y, i8 corner, Corner* next) {
    Corner* newNode = malloc(sizeof(Corner));
    newNode->x = x;
    newNode->y = y;
    newNode->corner = corner;
    newNode->next = next;
    return newNode;
}

void destroyCorners(Corner* node) {
    Corner* next = node->next;
    free(node);
    while(next != NULL) {
       node = next;
       next = node->next;
       free(node);
    }
}
