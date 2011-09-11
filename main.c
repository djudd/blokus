#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"

void init() {

}


int main(void) {
    init();
    printf("Done\n");

//    Cell* cells = addCell(3, 4, NULL);
//    i64* board = afterMove(empty(), 1, cells);
//
//    printf("%d\n", onBoardAndValidFor(board, 1, cells));
//
//    printf("%d\n", onBoardAndValidFor(empty(), 1, cells));
//
//    Cell* cells2 = addCell(3, 5, NULL);;
//    printf("%d\n", onBoardAndValidFor(board, 1, cells2));
//
//    printf("%d\n", onBoardAndValidFor(board, 2, cells2));
//
//    Cell* corners = availableCorners(empty(), 1);
//    while(corners != NULL) {
//        printf("%d, %d\n", corners->x, corners->y);
//        corners = corners->next;
//    }
//
//    board = afterMove(board, 4, addCell(0,BOARD_SIZE-1,NULL));
//
//    printf("---\n");
//
//    Cell* corners2 = availableCorners(board, 1);
//    while(corners2 != NULL) {
//        printf("%d, %d\n", corners2->x, corners2->y);
//        corners2 = corners2->next;
//    }
}
