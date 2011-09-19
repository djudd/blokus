#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
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

int nodesSearched = 0;
int nodesPruned = 0;
int maxTurnSearched = 0;

bool scoreLessFor(GameState* a, GameState* b, i8 player) {
    // TODO: Secondary comparison based on opponents' scores
    return a->scores[player] < b->scores[player];
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
        if (scoreLessFor(pivot, node, player)) {
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

float* terminalScores(GameState* state) {
    float* scores = malloc(NUM_PLAYERS*sizeof(float));
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = totalPieceSize;
        i32 pieces = (&(state->pieces))[p];
        for (i8 i=0; i<NUM_PIECES; i++) {
            if ((pieces & (((i32) 1) << i)) > 0)
                scores[p] -= pieceSizes[i];
        }
    }

    i8 winner = 0;
    float max = 0;
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        if (scores[p] > max) {
            max = scores[p];
            winner = p;
        }
    }

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        if (p == winner)
            scores[p] = 1;
        else
            scores[p] = 0;
    }

    return scores;
}

float* heuristicScores(GameState* state) {
    nodesSearched++; // for debugging
//    if (nodesSearched % 100000 == 0) {
//        printf("%s\n", toString(state->board));
//        printf("Nodes searched: %d\n", nodesSearched);
//    }

    float* scores = malloc(NUM_PLAYERS*sizeof(float));
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = totalPieceSize;
        i32 pieces = (&(state->pieces))[p];
        for (i8 i=0; i<NUM_PIECES; i++) {
            if ((pieces & (((i32) 1) << i)) > 0)
                scores[p] -= pieceSizes[i];
        }
    }

    float totalScore = 0;
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        totalScore += scores[p];
    }

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = scores[p] / totalScore;
    }

    return scores;
}

GameState* search(GameState* node, int depth) {
    if (maxTurnSearched < node->turn)
        maxTurnSearched = node->turn;

    if (depth <= 0) {
        node->scores = heuristicScores(node);
        return node;
    }

    GameState* child = children(node);

    if (child == NULL) {
        node->scores = terminalScores(node);
        return node;
    }

    GameState* current = child;
    do {
        GameState* grandChild = search(current, depth-1);
        current->scores = grandChild->scores;
        if (grandChild != current) // if we didn't short circuit, but actually searched a new set of children
            destroy(grandChild);

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

        printf("%s\n", toString(&(node->board)));
        printf("Nodes searched: %d\n", nodesSearched);
        printf("Max turn searched: %d\n", maxTurnSearched);
        nodesSearched = 0;
        maxTurnSearched = 0;

        node = node->parent;
        depth++;
    }

    return node; // care only about head
}

int main(void) {
    init();
    iterativeDeepeningSearch(newGame(), 4);
}
