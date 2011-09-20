#include <stdlib.h>
#include "defs.h"

i8* pieceSizes;
i32 totalPieceSize;

void setPieceSizes() {
    pieceSizes = malloc(NUM_PIECES*sizeof(i8));
    pieceSizes[0] = 1;
    pieceSizes[1] = 2;
    pieceSizes[2] = 3;
    pieceSizes[3] = 3;
    pieceSizes[4] = 4;
    pieceSizes[5] = 4;
    pieceSizes[6] = 4;
    pieceSizes[7] = 4;
    pieceSizes[8] = 4;
    for (int i=9; i<NUM_PIECES; i++)
        pieceSizes[i] = 5;

    totalPieceSize = 0;
    for (int i=0; i<NUM_PIECES; i++)
        totalPieceSize += pieceSizes[i];
}