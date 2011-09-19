#define i8 signed char
#define i32 unsigned int
#define i64 unsigned long long

#define BOARD_SIZE 20
#define NUM_PIECES 21
#define NUM_PLAYERS 4
#define NUM_CORNERS 4

#define UPPER_LEFT 0
#define LOWER_LEFT 1
#define LOWER_RIGHT 2
#define UPPER_RIGHT 3

typedef struct cell {
    i8 x, y;
    struct cell * next;
} Cell;

Cell* addCell(i8 x, i8 y, Cell* next);

typedef struct corner {
    i8 x, y;
    i8 corner;
    i64 bitmap;
    struct corner * next;
} Corner;

Corner* addCorner(i8 x, i8 y, i8 corner, i64 bitmap, Corner* next);

void initCornerStorage();
void destroyCorners();

typedef struct placement {
    Cell* cells;
    i64 bitmap;
    struct placement * next;
} Placement;

Placement*** placements;

void generatePlacements();

i8* pieceSizes;
float totalPieceSize;
void setPieceSizes();

i64 bit(i8 x, i8 y);

char* toString(i64* board);

// TODO this probably shouldn't have pointers for board (& scores?), it should have a field per array value and we should use pointer arithmetic
typedef struct state {
    i8 turn;
    i64* board;
    i32 pieces;
    i32 pieces_1;
    i32 pieces_2;
    i32 pieces_3;
    float* scores;
    struct state * parent;
    struct state * next;
} GameState;

GameState* newGame();
void destroy(GameState* state);

GameState* children(GameState* state);