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

i8* pieceSizes;
i32 totalPieceSize;
void setPieceSizes();

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
i64 bit(i8 x, i8 y);

void assign(i64* board, i8 player, Corner* origin, Cell* cell);
Corner* availableCorners(i64* board, i8 player, i8 turn);
char* toString(i64* board);

typedef struct state {
    i8 turn;
    i64 board;
    i64 board_1;
    i64 board_2;
    i64 board_3;
    i64 board_4;
    i64 board_5;
    i64 board_6;
    i64 board_7;
    i64 board_8;
    i64 board_9;
    i64 board_10;
    i64 board_11;
    i64 board_12;
    i64 board_13;
    i64 board_14;
    i64 board_15;
    i64 board_16;
    i64 board_17;
    i64 board_18;
    i64 board_19;
    i32 pieces;
    i32 pieces_1;
    i32 pieces_2;
    i32 pieces_3;
    float scores;
    float scores_1;
    float scores_2;
    float scores_3;
    struct state * parent;
    struct state * next;
} GameState;

GameState* newGame();
void destroy(GameState* state);
GameState* children(GameState* state);

void setHeuristicScores(GameState* state);
void setTerminalScores(GameState* state);

GameState* iterativeDeepeningSearch(GameState* node, int maxDepth);