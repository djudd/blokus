#include <stdlib.h>
#include <stdio.h>
#include "defs.h"

void init() {
    generatePlacements();
    initCornerStorage();
    setPieceSizes();
}

int main(void) {
    init();
    iterativeDeepeningSearch(newGame(), 4);
}
