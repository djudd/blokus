#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"

Cell* addCell(int x, int y, Cell* next) {
    Cell* newNode = malloc(sizeof(Cell));
    newNode->x = x;
    newNode->y = y;
    newNode->next = next;
    return newNode;
}

void destroy(Cell* cell) {
    Cell* next = cell->next;
    free(cell);
    while(next != NULL) {
       cell = next;
       next = cell->next;
       free(cell);
    }
}
