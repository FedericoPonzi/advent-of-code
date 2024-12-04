namespace aoc2024;

public class Day4
{
    public static int search(int r, int c, char[][] board, String word, (int, int)positions)
    {
        int nextR = r;
        int nextC = c;
        for (int p = 0; p < word.Length; p++)
        {
            var (rows, cols) = positions;
            nextR += rows;
            nextC += cols;
            if (nextR < 0 || nextR >= board.Length) return 0;
            if (nextC < 0 || nextC >= board[0].Length) return 0;
            if (!board[nextR][nextC].Equals(word[p])) return 0;
        }

        return 1;
    }

    public static int solve1(String path)
    {
        if (!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path).Select(l => l.ToCharArray()).ToArray();

        var directions = new List<(int, int)>();
        for (int r = -1; r <= 1; r++)
        {
            for (int c = -1; c <= 1; c++)
            {
                directions.Add((r, c));
            }
        }

        var total = 0;
        for (int r = 0; r < lines.Length; r++)
        {
            var line = lines[r];
            for (int c = 0; c < line.Length; c++)
            {
                var cell = line[c];
                if (!cell.Equals('X'))
                {
                    continue;
                }
                var word = "MAS"; // X already matched
                foreach (var direction in directions)
                {
                    total += search(r, c, lines, word, direction);
                }
            }
        }

        return total;
    }

    public static int solve2(String path)
    {
        if (!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path).Select(l => l.ToCharArray()).ToArray();
        var directions = new List<(int, int)>();
        for (int r = -1; r <= 1; r++)
        {
            for (int c = -1; c <= 1; c++)
            {
                if (c != 0 && r != 0) // exclude horizontal and vertical changes
                {
                    directions.Add((r, c));
                }
            }
        }

        var setOfA = new HashSet<(int,int)>();
        var foundX = new HashSet<(int,int)>();
        for (int r = 0; r < lines.Length; r++)
        {
            var line = lines[r];
            for (int c = 0; c < line.Length; c++)
            {
                var cell = line[c];
                if (!cell.Equals('M'))
                {
                    continue;
                }
                var word = "AS"; // M already matched
                foreach (var direction in directions)
                {
                    if (search(r, c, lines, word, direction) == 1)
                    {
                        var (rows, cols) = direction;
                        rows += r;
                        cols += c;
                        if (setOfA.Contains((rows, cols)) && !foundX.Contains((rows, cols)))
                        {
                            foundX.Add((rows, cols));
                        }
                        setOfA.Add((rows, cols));
                    }
                }
            }
        }
        return foundX.Count;
    }
}
