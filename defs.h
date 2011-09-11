#define true 1
#define false 0

#define i16 unsigned int
#define i64 unsigned long long

#define BOARD_SIZE 20
#define NUM_PIECES 20

typedef struct cell {
    int x, y;
    struct cell * next;
} Cell;

Cell* addCell(int x, int y, Cell* next);

void destroy(Cell* cell);