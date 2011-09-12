#define i8 char
#define i32 unsigned int
#define i64 unsigned long long

#define BOARD_SIZE 20
#define NUM_PIECES 20
#define NUM_PLAYERS 4
#define NUM_CORNER 4

#define UPPER_LEFT 0
#define LOWER_LEFT 1
#define LOWER_RIGHT 2
#define UPPER_RIGHT 3

typedef struct cell {
    i8 x, y;
    struct cell * next;
} Cell;

Cell* addCell(i8 x, i8 y, Cell* next);

void destroyCells(Cell* cell);

typedef struct corner {
    i8 x, y;
    i8 corner;
    i64 bitmap;
    struct corner * next;
} Corner;

Corner* addCorner(i8 x, i8 y, i8 corner, Corner* next);

void destroyCorners(Corner* cell);

typedef struct placement {
    Cell* cells;
    i64 bitmap;
    struct placement * next;
} Placement;

Placement*** placements;

void generatePlacements();

i64 bit(i8 x, i8 y);

typedef struct state {
    i8 turn;
    i64* board;
    i32* pieces;
    struct state * next;
} GameState;