#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "defs.h"

int nodesSearched = 0;
int nodesPruned = 0;
int maxTurnSearched = 0;

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

i32 countPieceSizes(GameState* state, i8 player) {
    i32 result = totalPieceSize;
    i32 pieces = (&(state->pieces))[player];
    for (i8 i=0; i<NUM_PIECES; i++) {
        if ((pieces & (((i32) 1) << i)) > 0)
            result -= pieceSizes[i];
    }
    return result;
}

void normalizeTerminalScores(float* scores) {
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
}

void setTerminalScores(GameState* state) {
    float* scores = &(state->scores);

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = (float) countPieceSizes(state, p);
    }

    normalizeTerminalScores(scores);
}

void normalizeScores(float* scores) {
    float totalScore = 0;
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        totalScore += scores[p];
    }

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = scores[p] / totalScore;
    }
}

void setHeuristicScores(GameState* state) {
    nodesSearched++; // for debugging

    float* scores = &(state->scores);

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = (float) countPieceSizes(state, p);
    }

    normalizeScores(scores);
}

GameState* search(GameState* node, int depth) {
    #IFNDEF NDEBUG
    if (maxTurnSearched < node->turn)
        maxTurnSearched = node->turn;

    if (depth <= 0) {
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
        copyScores(grandChild, current);
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
