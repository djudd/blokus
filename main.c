#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
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
        printf("%s\n", toString(child->board));
        child = child->next;
    }
}

/*
void nPlies(int n) {
    int nodesExplored = 0;

    GameState* current = newGame();
    GameState* firstChild = NULL;
    GameState* lastChild = NULL;

    for (int i=0; i<n+1; i++) {
        while (current != NULL) {
            nodesExplored++;
            if (nodesExplored % 10000 == 0)
                printf("%s\n", toString(current->board));

            GameState* currentChildren = children(current);

            if (firstChild == NULL)
                firstChild = currentChildren;

            if (lastChild == NULL)
                lastChild = currentChildren;
            else
                lastChild->next = currentChildren;

            while (lastChild->next != NULL)
                lastChild = lastChild->next;

            current = current->next;
        }

        current = firstChild;
        firstChild = NULL;
        lastChild = NULL;
    }

    printf("Explored %d nodes through ply %d\n", nodesExplored, n);
}*/

int nodesSearched = 0;
int nodesPruned = 0;

typedef struct node {
    GameState* state;
    float* scores;
    struct node* next;
} GameNode;

GameNode* makeNode(GameState* state, float* scores, GameNode* next) {
    GameNode* node = malloc(sizeof(GameNode));
    node->state = state;
    node->scores = scores;
    node->next = next;
    return node;
}

void destroy(GameNode* node) {
    if (node == NULL)
        return;

    destroy(node->next);

    free(node->state->board);
    free(node->state->pieces);
    // we don't free node->state->next because we'll do that when freeing another node
    free(node->state);

    free(node->scores);
    free(node);
}

bool scoreLessFor(GameNode* a, GameNode* b, i8 player) {
    // TODO: Secondary comparison based on opponents' scores
    return a->scores[player] < b->scores[player];
}

GameNode* sort(GameNode* node) {
    i8 player = node->state->turn % 4; // here 0-indexed, not 1-indexed

    GameNode* pivot = node;
    GameNode* less = NULL;
    int lessCount = 0;
    GameNode* more = NULL;
    int moreCount = 0;

    node = node->next;
    while (node != NULL) {
        GameNode* next = node->next;
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

    GameNode* smallestMore = more;
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
        i32 pieces = state->pieces[p];
        for (i8 i=0; i<NUM_PIECES; i++) {
            if ((pieces & (1 << i)) > 0)
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
    float* scores = malloc(NUM_PLAYERS*sizeof(float));
    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = totalPieceSize;
        i32 pieces = state->pieces[p];
        for (i8 i=0; i<NUM_PIECES; i++) {
            if ((pieces & (1 << i)) > 0)
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

GameNode* search(GameState* node, GameState* child, int depth) {
    if (child == NULL) {
        return makeNode(NULL, terminalScores(node), NULL);
    }

    if (depth <= 0) {
        return makeNode(NULL, heuristicScores(node), NULL);
    }

    GameNode* scoredChild = NULL;
    do {
        nodesSearched++; // for debugging

        GameNode* grandChild = search(child, children(child), depth-1);
        destroy(grandChild->next); // only care about head

        scoredChild = makeNode(
            child,
            grandChild->scores,
            scoredChild
        );

        child = child->next;
    } while (child != NULL);

    return sort(scoredChild);
}

GameState* iterativeDeepeningSearch(GameState* node, int maxDepth) {
    GameState* child = children(node);
    int depth = 1;

    while (depth <= maxDepth) {
        // TODO: Use sorted order
        child = search(
            node,
            child,
            depth
        )->state;

        printf("%s\n", toString(child->board));

        depth++;
    }

    return child; // care only about head
}

int main(void) {
    init();
    iterativeDeepeningSearch(newGame(), 2);
}
