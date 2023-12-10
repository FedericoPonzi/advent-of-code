#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


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
   fclose(fp);
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
                //printf("Found c, y: %d, x: %d\n", i, k);
                pair ret = {.y = k, .x = i};
                return ret;
            }
        }
    }
}

int run_maze(char *maze, int size, pair position, char starter_kind, int *maze_pipes) {
    char cur = starter_kind;
    pair visited = {.x = position.x, .y = position.y};
    pair current = {.x = position.x, .y = position.y};
    int ret = 0;
    while (cur != 'S') {
        ret++;
        if (ret > size * size) {
            printf("current ret is greater than max length!?, %d", ret);
            exit(1);
        }
        if (cur == 'F') {
            pair next = {.x = current.x, .y = current.y + 1};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x + 1, .y = current.y};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '-') {
            pair next = {.x = current.x, .y = current.y + 1};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x, .y = current.y - 1};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '|') {
            pair next = {.x = current.x + 1, .y = current.y};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x - 1, .y = current.y};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else if (cur == 'J') {
            pair next = {.x = current.x - 1, .y = current.y};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x, .y = current.y - 1};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else if (cur == '7') {
            pair next = {.x = current.x, .y = current.y - 1};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x + 1, .y = current.y};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else if (cur == 'L') {
            pair next = {.x = current.x - 1, .y = current.y};
            if (!memcmp(&next, &visited, sizeof(next))) {
                next = (pair) {.x = current.x, .y = current.y + 1};
            }
            visited = current;
            current = next;
            cur = *(maze + current.x * size + current.y);
        } else {
            printf("unknown char found: '%c'\n", cur);
            return 0;
        }
        if (maze_pipes != NULL) {
            *(maze_pipes + current.x * size + current.y) = 1;
        }
    }
    return ret;
}

int solve(char *filename, int size, char starter_kind) {
    char *maze = malloc(size * size);
    load_graph(filename, size, maze);
    pair p = find_starter(maze, size);
    int ret = run_maze(maze, size, p, starter_kind, NULL);
    free(maze);
    return ret / 2;
}

int solve2(char *filename, int size, char starter_kind) {
    char *maze = malloc(size * size);
    int *pipes_map = calloc(size * size, sizeof(int));
    load_graph(filename, size, maze);
    pair p = find_starter(maze, size);
    run_maze(maze, size, p, starter_kind, pipes_map);
    /*for (int i = 0; i < size; i++) {
        for (int k = 0; k < size; k++) {
            printf("%d ", *(pipes_map + i * size + k));
        }
        printf("\n");
    }*/
    int ret = 0;
    for (int i = 0; i < size; i++) {
        bool is_inside = false;
        for (int k = 0; k < size; k++) {
            int print_me = i == 7;
            char target = *(maze + i * size + k);
            int is_pipe_tile = *(pipes_map + i * size + k) == 1;
            if (is_pipe_tile) {
                //printf("P ");
                if (target == 'S') {
                    target = starter_kind;
                }
                if (target == '|' || target == 'J' || target == 'L') {
                    is_inside = !is_inside;
                }
            } else if (is_inside) {
                //printf("I ");
                ret++;
            } else {
                //      printf(". ");
            }
        }
        //printf("\n");
    }

    free(maze);
    free(pipes_map);
    return ret;
}

int main() {
    int ret;
    // the starters were found manually :)
    printf("Received: %d, expected: 8.\n", solve("/home/fponzi/dev/advent-of-code/2023/day-10/example.txt", 5, 'F'));
    printf("Received: %d, expected: 7030.\n", solve("/home/fponzi/dev/advent-of-code/2023/day-10/input.txt", 140, 'J'));
    printf("Received: %d, expected: 4.\n",
           solve2("/home/fponzi/dev/advent-of-code/2023/day-10/example2-a.txt", 11, 'F'));
    printf("Received: %d, expected: 8.\n",
           solve2("/home/fponzi/dev/advent-of-code/2023/day-10/example2-b.txt", 20, 'F'));
    printf("Received: %d, expected: 10.\n",
           solve2("/home/fponzi/dev/advent-of-code/2023/day-10/example2-c.txt", 20, '7'));
    printf("Received: %d, expected: 4.\n",
           solve2("/home/fponzi/dev/advent-of-code/2023/day-10/example2-d.txt", 10, '|'));
    printf("Received: %d, expected: 285.\n", solve2("/home/fponzi/dev/advent-of-code/2023/day-10/input.txt", 140, 'J'));
    return 0;
}
