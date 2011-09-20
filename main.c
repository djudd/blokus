#include <stdlib.h>
#include <stdio.h>
#include "defs.h"

void init() {
    generatePlacements();
    initCornerStorage();
    setPieceSizes();
}

// for debugging
void printInitialMoves() {
    GameState* game = newGame();
    GameState* child = children(game);
    while (child != NULL) {
        printf("%s\n", toString(&(child->board)));
        child = child->next;
    }
}

int main(void) {
    init();
    iterativeDeepeningSearch(newGame(), 4);
}
