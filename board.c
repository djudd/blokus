#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define true 1
#define false 0

#define i64 unsigned long long

const int BOARD_SIZE = 20;

i64 pow5(int n) {
    // TODO: This could be a switch statement...
    i64 p = 5;
    i64 r = 1;
    while (n > 0)
    {
        if (n % 2 == 1)
            r *= p;
        p *= p;
        n /= 2;
    }

    return r;
}

typedef struct node {
    int x, y;
    struct node * next;
} Node;

Node* addNode(int x, int y, Node* next) {
    Node* newNode = malloc(sizeof(Node));
    newNode->x = x;
    newNode->y = y;
    newNode->next = next;
    return newNode;
}

void destroy(Node* node) {
    Node* next = node->next;
    free(node);
    while(next != NULL) {
       node = next;
       next = node->next;
       free(node);
    }
}

void setOwner(i64* board, short player, int x, int y) {
    i64 old = board[x];
    board[x] = board[x] + (pow5(y) * player);
    printf("setting (%d, %d) owner to %d; row was %llu, now %llu\n", x, y, player, old, board[x]);
}

short owner(i64* board, int x, int y) {
    return (board[x] / pow5(y)) % 5;
}

int hasOwner(i64* board, short player, int x, int y) {
    return (board[x] / pow5(y)) % 5 == player;
}

int hasAnyOwner(i64* board, int x, int y) {
    return (board[x] / pow5(y)) % 5 > 0;
}

void assign(i64* board, short player, Node* node) {
    while (node != NULL) {
        setOwner(board, player, node->x, node->y);
        node = node->next;
    }
}

int touchesCorner(i64* board, short player, int x, int y) {
    if (x+1 < BOARD_SIZE) {
        if (y+1 < BOARD_SIZE && hasOwner(board, player, x+1, y+1)) {
            printf("(%d, %d) touches (%d, %d) owned by %d\n", x, y, x+1, y+1, owner(board, x+1,y+1));
            return true;
        }
        if (y-1 >= 0 && hasOwner(board, player, x+1, y-1)) {
            printf("(%d, %d) touches (%d, %d) owned by %d\n", x, y, x+1, y-1, owner(board, x+1,y-1));
            return true;
        }
    }
    if (x-1 >= 0) {
        if (y+1 < BOARD_SIZE && hasOwner(board, player, x-1, y+1)) {
            printf("(%d, %d) touches (%d, %d) owned by %d\n", x, y, x-1, y+1, owner(board, x-1,y+1));
            return true;
        }
        if (y-1 >= 0 && hasOwner(board, player, x-1, y-1)) {
            printf("(%d, %d) touches (%d, %d) owned by %d\n", x, y, x-1, y-1, owner(board, x-1,y-1));
            return true;
        }
    }

    return false;
}

int touchesSide(i64* board, short player, int x, int y) {
    if (x+1 < BOARD_SIZE && hasOwner(board, player, x+1, y))
        return true;
    if (x-1 >= 0 && hasOwner(board, player, x-1, y))
        return true;
    if (y+1 < BOARD_SIZE && hasOwner(board, player, x, y+1))
        return true;
    if (y-1 >= 0 && hasOwner(board, player, x, y-1))
        return true;

    return false;
}

int _onBoardAndValidFor(i64* board, short player, int x, int y) {
    if (x < 0 || x >= BOARD_SIZE || y < 0 || y >= BOARD_SIZE)
        return false;

    if (hasAnyOwner(board, x, y))
        return false;

    if (touchesSide(board, player, x, y))
        return false;

    return true;
}

int onBoardAndValidFor(i64* board, short player, Node* node) {
    while (node != NULL) {
        if (!_onBoardAndValidFor(board, player, node->x, node->y))
            return false;
        node = node->next;
    }
    return true;
}

int isFirstTurn(i64* board) {
    return !hasAnyOwner(board, 0, BOARD_SIZE-1); // assumes player 1 starts at (0,0), 2 at (19,0), 3 at (19,19), and 4 at (0,19)
}

Node* availableCorners(i64* board, short player) {
    if (isFirstTurn(board)) {
        switch(player) {
            case 1: return addNode(0,0,NULL);
            case 2: return addNode(BOARD_SIZE-1,0,NULL);
            case 3: return addNode(BOARD_SIZE-1,BOARD_SIZE-1,NULL);
            case 4: return addNode(0,BOARD_SIZE-1,NULL);
        }
    }

    Node* node = NULL;
    for (int x=0; x<BOARD_SIZE; x++) {
        for (int y=0; y<BOARD_SIZE; y++) {
//            printf("At (%d,%d) hasAnyOwner=%d, touchesCorner=%d\n", x, y, hasAnyOwner(board,x,y), touchesCorner(board, player, x, y));
            if (!hasAnyOwner(board, x, y) && touchesCorner(board, player, x, y)) {
                node = addNode(x, y, node);
            }
        }
    }

    return node;
}

i64* afterMove(i64* board, short player, Node* node) {
    i64* child = (i64*) malloc(BOARD_SIZE * sizeof(i64));
    for (int i=0; i<BOARD_SIZE; i++) printf("board[%d]: %llu\n", i, board[i]);
    memcpy(child, board, BOARD_SIZE);
    for (int i=0; i<BOARD_SIZE; i++) printf("child[%d]: %llu\n", i, child[i]);
    assign(child, player, node);
    return child;
}

i64* empty() {
    i64* board = (i64*) malloc(BOARD_SIZE * sizeof(i64));
    for (int i=0; i<BOARD_SIZE; i++)
        board[i] = 0;
    return board;
}

int main(void) {
    Node* cells = addNode(3, 4, NULL);
    i64* board = afterMove(empty(), 1, cells);

    printf("%d\n", onBoardAndValidFor(board, 1, cells));

    printf("%d\n", onBoardAndValidFor(empty(), 1, cells));

    Node* cells2 = addNode(3, 5, NULL);;
    printf("%d\n", onBoardAndValidFor(board, 1, cells2));

    printf("%d\n", onBoardAndValidFor(board, 2, cells2));

    Node* corners = availableCorners(empty(), 1);
    while(corners != NULL) {
        printf("%d, %d\n", corners->x, corners->y);
        corners = corners->next;
    }

    board = afterMove(board, 4, addNode(0,BOARD_SIZE-1,NULL));

    printf("---\n");

    Node* corners2 = availableCorners(board, 1);
    while(corners2 != NULL) {
        printf("%d, %d\n", corners2->x, corners2->y);
        corners2 = corners2->next;
    }
}
