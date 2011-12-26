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

typedef i8 Direction;
typedef i8 Offset;
typedef i8 Coord;
typedef i8 Player;

i8* pieceSizes;
i32 totalPieceSize;
void setPieceSizes();

typedef struct cell {
    Offset x, y;
    struct cell * next;
} Cell;

Cell* addCell(Offset x, Offset y, Cell* next);

typedef struct corner {
    Offset x, y;
    Direction corner;
    i64 bitmap;
    struct corner * next;
} Corner;

Corner* addCorner(Offset x, Offset y, Direction corner, i64 bitmap, Corner* next);

void initCornerStorage();
void destroyCorners();

typedef struct placement {
    Cell* cells;
    Corner* corners;
    i64 bitmap;
    struct placement * next;
} Placement;

Placement*** placements;
void generatePlacements();
i64 bit(Coord x, Coord y);

void assign(i64* board, Player player, Corner* origin, Cell* cell);
void calculateCornerValidNeighborhoods(Corner* corners, i64* board, Player player);
Corner* getNextCornersForNonMovingPlayer(Corner* corners, i64* board);
Corner* getNextCornersForMovingPlayer(Corner* corners, i64* board, Player player, Corner* origin, Corner* newCorners);
char* toString(i64* board);

// board, pieces, scores, and corners are really arrays
// we embed them directly in the struct in order to do just one malloc & free
// this isn't necessarily portable
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
    Corner* corners;
    Corner* corners_1;
    Corner* corners_2;
    Corner* corners_3;
    struct state * parent;
    struct state * next;
} GameState;

GameState* newGame();
void destroy(GameState* state);
void initChildrenSearch(GameState* state);
void cleanupChildrenSearch(GameState* state);
GameState* nextChild(GameState* state);

void setHeuristicScores(GameState* state);
void setTerminalScores(GameState* state);

GameState* iterativeDeepeningSearch(GameState* node, int maxDepth);
