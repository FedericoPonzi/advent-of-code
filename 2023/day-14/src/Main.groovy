static def solve(ArrayList<ArrayList<String>> l) {
    int res = 0;
    for (int row = 0; row < l.size(); row++) {
        int res_row = 0;
        for (int col = 0; col < l[0].size(); col++) {
            if (l[row][col] == ".") {
                for (int next_row = row + 1; next_row < l.size(); next_row++) {
                    if (next_row < l.size() && l[next_row][col] == "O") {
                        l[row][col] = "O";
                        l[next_row][col] = ".";
                        break;
                    }
                    if(l[next_row][col] == "#"){
                        break;
                    }
                }
            }
            if (l[row][col] == "O") {
                res_row++;
            }
        }
        res += res_row * (l.size() - row);
    }
    return res;
}

static void main(String[] args) {
    def lines = []
    new File('/home/fponzi/dev/advent-of-code/2023/day-14/input.txt').eachLine { line ->
        lines.add(line.split(""))
    }
    println solve(lines)
}