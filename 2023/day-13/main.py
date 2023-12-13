def find_col_mirror(pattern, col, max_length):
    for i in range(0, len(pattern)):
        left = pattern[i][col - max_length:col]
        right = pattern[i][col:col + max_length]
        print(''.join(list(reversed(left))), "-", right)
        if ''.join(list(reversed(left))) != right:
            return False
    return True


def find_mirror(pattern):
    # just need to check a single row
    for col in range(1, len(pattern[0])):
        max_length = min(len(pattern[0][col:]), len(pattern[0][:col]))
        left = pattern[0][col - max_length:col]
        right = pattern[0][col:col + max_length]
        if ''.join(list(reversed(left))) == right:
            if find_col_mirror(pattern, col, max_length):
                return col
    return None


def count_sol(cols_left, rows_above):
    return cols_left + 100 * rows_above


def solve1(filename):
    columns = 0
    rows = 0
    with open(filename, 'r') as file:
        data = file.read().split('\n\n')

        for block in data:
            lines = block.strip().split('\n')
            pattern = [line for line in lines]
            mirror = find_mirror(pattern)
            if mirror is not None:
                columns += mirror
                continue
            pattern_transposed = list(map(list, zip(*pattern)))
            pattern_transposed = ["".join(line) for line in pattern_transposed]
            mirror = find_mirror(pattern_transposed)
            if mirror is None:
                raise Exception("No mirror found :(", pattern)
            rows += mirror
    return count_sol(columns, rows)


if __name__ == '__main__':
    #assert count_sol(5, 4) == 405
    print(solve1("example.txt"))
    print(solve1("input.txt"))
