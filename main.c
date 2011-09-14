#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"

void init() {
    generatePlacements();
    initCornerStorage();
}

int main(void) {
    init();

    GameState* game = newGame();
    GameState* child = children(game);
    while (child != NULL) {
        printf("Found child, pieces[0] = %u\n", child->pieces[0]);
        child = child->next;
    }

    printf("Done\n");

}
