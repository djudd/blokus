#include <stdlib.h>
#include "defs.h"

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
    float* scores = &(state->scores);

    for (i8 p=0; p<NUM_PLAYERS; p++) {
        scores[p] = (float) countPieceSizes(state, p);
    }

    normalizeScores(scores);
}
