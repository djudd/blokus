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
    printf("Done\n");
}
