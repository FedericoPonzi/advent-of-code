#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * 1. parse the map.
 * 2. start bfs, mark all the distances.
 * 3. find the highest distance.
 *
 *
 * @return
 */

typedef enum {
    North,
    South,
    East,
    West
} Direction;

void load_graph(char *filename, int size, char *);


/**
 *
 * @return "S" the start node.
 */

void load_graph(char *filename, int size, char *ret) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }

    int row = 0;
    while (row < size) {
        int res = fread(ret + row * size, 1, size, fp);
        if (res != size) {
            fprintf(stderr, "fread() failed: %d, row= %d, total = %d\n", res, row, size);
            exit(EXIT_FAILURE);
        }
        // skip EOF
        fseek(fp, 1, SEEK_CUR);
        row++;
    }
}

void print_maze(char *maze, int size) {
    for (int i = 0; i < size; i++) {
        for (int k = 0; k < size; k++) {
            printf("%c ", *(maze + i * size + k));
        }
        printf("\n");
    }
}

typedef struct {
    int x;
    int y;
} pair;

pair find_starter(char *maze, int size) {
    for (int i = 0; i < size; i++) {
        for (int k = 0; k < size; k++) {
            char c = *(maze + i * size + k);
            if (c == 'S') {
                printf("Found c, y: %d, x: %d\n", i, k);
                pair ret = {.y = k, .x = i};
                return ret;
            }
        }
    }
}


int canGoDirection(char *maze, int size, pair pos, Direction dir) {
    if (dir == North) {
        const char final = *(maze + (pos.y - 1) * size + pos.x);
        return pos.y - 1 >= 0 && (final == 'F' || final == '7' || final == '|');
    }
    if (dir == East) {
        const char final = *(maze + (pos.y - 1) * size + pos.x);
        return pos.y - 1 >= 0 && (final == 'J' || final == '7' || final == '-');
    }
}

int getStarterKind(char *maze, int size, pair pos) {
    if (canGoDirection(maze, size, pos, East)) {
        printf("Can go east");
    }
}

int run_maze(char *maze, int size, pair position, char starter_kind) {
    char cur = starter_kind;
    pair visited = {.x = position.x, .y = position.y};
    pair current = {.x = position.x, .y = position.y};
    int ret = 0;
    while (cur != 'S') {
        ret++;
        if(ret > size*size){
            printf("current ret is greater than max length!?, %d", ret);
            exit(1);
        }
        //printf("Searhing for next pipe, curr: %c (%d, %d) \n", cur, current.x, current.y);
        if (cur == 'F') {
            pair next = {.x = current.x, .y = current.y + 1};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x + 1, .y = current.y};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '-') {
            pair next = {.x = current.x, .y = current.y + 1};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x, .y = current.y - 1};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '|') {
            pair next = {.x = current.x + 1, .y = current.y};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x - 1, .y = current.y};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else if (cur == 'J') {
            pair next = {.x = current.x - 1, .y = current.y};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x, .y = current.y - 1};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '7') {
            pair next = {.x = current.x, .y = current.y - 1};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x + 1, .y = current.y};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else if (cur == 'L') {
            pair next = {.x = current.x - 1, .y = current.y};
            if (memcmp(&next, &visited, sizeof(next))) {
                visited = current;
                current = next;
            } else {
                visited = current;
                pair tmp = {.x = current.x, .y = current.y + 1};
                current = tmp;
            }
            cur = *(maze + current.x * size + current.y);
        } else {
            printf("unknown char found: %c\n", cur);

            return 0;
        }
        //printf("Next step: %c (%d, %d)\n", cur, current.x, current.y);

    }
    //printf("Found the end pipe!\n");
    return ret;
}

int solve(char *filename, int size, char starter_kind) {
    char *maze = malloc(size * size);
    load_graph(filename, size, maze);
    pair p = find_starter(maze, size);
    int ret = run_maze(maze, size, p, starter_kind);
    free(maze);
    return ret/2;
}

int main() {
    int ret;
    ret = solve("/home/fponzi/dev/advent-of-code/2023/day-10/example.txt", 5, 'F');
    printf("Received: %d, expected: 8.", ret);
    ret = solve("/home/fponzi/dev/advent-of-code/2023/day-10/input.txt", 140, 'J');
    printf("Received: %d, expected: 7030.", ret);
    return 0;
}
