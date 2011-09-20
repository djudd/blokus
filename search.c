#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "defs.h"

#ifndef NDEBUG
    int nodesSearched = 0;
    int nodesPruned = 0;
    int maxTurnSearched = 0;
#endif

bool scoredLess(GameState* a, GameState* b, i8 player) {
    // TODO: Secondary comparison based on opponents' scores
    return (&(a->scores))[player] < (&(b->scores))[player];
}

GameState* sort(GameState* node) {
    i8 player = node->turn % 4; // here 0-indexed, not 1-indexed

    GameState* pivot = node;
    GameState* less = NULL;
    int lessCount = 0;
    GameState* more = NULL;
    int moreCount = 0;

    node = node->next;
    while (node != NULL) {
        GameState* next = node->next;
        if (scoredLess(pivot, node, player)) {
            node->next = less;
            less = node;
            lessCount++;
        }
        else {
            node->next = more;
            more = node;
            moreCount++;
        }
        node = next;
    }

    if (lessCount > 1)
        less = sort(less);
    if (moreCount > 1)
        more = sort(more);

    pivot->next = less;

    GameState* smallestMore = more;
    while (smallestMore != NULL && smallestMore->next != NULL)
        smallestMore = smallestMore->next;
    if (smallestMore != NULL) {
        smallestMore->next = pivot;
        return more;
    }
    else {
        return pivot;
    }
}

void copyScores(GameState* from, GameState* to) {
    float* fScores = &(from->scores);
    float* tScores = &(to->scores);
    for (i8 i=0; i<NUM_PLAYERS; i++) {
        tScores[i] = fScores[i];
    }
}

GameState* search(GameState* node, int depth) {
    #ifndef NDEBUG
        if (maxTurnSearched < node->turn)
            maxTurnSearched = node->turn;

        nodesSearched++;
    #endif

    if (depth <= 0) {
        // heuristic scores were already calculated
        return node;
    }

    GameState* child = children(node);

    if (child == NULL) {
        setTerminalScores(node);
        return node;
    }

    GameState* current = child;
    do {
        GameState* grandChild = search(current, depth-1);
        if (grandChild != current) { // if we didn't short circuit, but actually searched a new set of children
            copyScores(grandChild, current);
            destroy(grandChild);
        }

        current = current->next;
    } while (current != NULL);

    return sort(child);
}

GameState* iterativeDeepeningSearch(GameState* node, int maxDepth) {
    int depth = 1;

    while (depth <= maxDepth) {
        // TODO: Use sorted order
        node = search(
            node,
            depth
        );

        #ifndef NDEBUG
            printf("%s\n", toString(&(node->board)));
            printf("Nodes searched: %d\n", nodesSearched);
            printf("Max turn searched: %d\n", maxTurnSearched);
            nodesSearched = 0;
            maxTurnSearched = 0;
        #endif

        node = node->parent;
        depth++;
    }

    return node; // care only about head
}
