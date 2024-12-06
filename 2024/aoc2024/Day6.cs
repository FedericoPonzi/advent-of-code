using System.Diagnostics;

namespace aoc2024;

public class Day6
{
    public static int solve1(string path)
    {
        string[] map = File.ReadAllLines(path);

        int[] dirX = { -1, 0, 1, 0 };
        int[] dirY = { 0, 1, 0, -1 };

        int x = -1, y = -1;

        for (int i = 0; i < map.Length; i++)
        {
            for (int j = 0; j < map[i].Length; j++)
            {
                if (map[i][j] == '^')
                {
                    x = i;
                    y = j;
                    break;
                }
            }

            if (x != -1) break;
        }

        int direction = 0; // Initial direction is up (index 0 for up)

        HashSet<(int, int)> visited = new HashSet<(int, int)>();
        visited.Add((x, y));

        while (true)
        {
            int newX = x + dirX[direction];
            int newY = y + dirY[direction];

            if (newX < 0 || newX >= map.Length || newY < 0 || newY >= map[0].Length)
                break;

            if (map[newX][newY] == '#')
            {
                direction = (direction + 1) % 4;
            }
            else
            {
                x = newX;
                y = newY;
                visited.Add((x, y));
            }
        }

        return visited.Count;
    }

    public static int solve2(string path)
    {
        var map = File.ReadAllLines(path).Select(l => l.ToCharArray()).ToArray();
        int[] dirX = { -1, 0, 1, 0 }; // Movement directions (up, right, down, left)
        int[] dirY = { 0, 1, 0, -1 };

        int guardX = -1, guardY = -1;

        for (int i = 0; i < map.Length; i++)
        {
            for (int j = 0; j < map[i].Length; j++)
            {
                if (map[i][j] == '^')
                {
                    guardX = i;
                    guardY = j;
                    break;
                }
            }

            if (guardX != -1) break;
        }

        int ret = 0;

        for (int i = 0; i < map.Length; i++)
        {
            for (int j = 0; j < map[0].Length; j++)
            {
                if (map[i][j] != '.') continue;
                map[i][j] = '#';

                var visited = new HashSet<(int, int, int)>();
                int direction = 0; // Start facing up
                int x = guardX;
                int y = guardY;

                while (true)
                {
                    int newX = x + dirX[direction];
                    int newY = y + dirY[direction];

                    if (newX < 0 || newX >= map.Length || newY < 0 || newY >= map[0].Length)
                        break;

                    if (map[newX][newY] == '#')
                    {
                        direction = (direction + 1) % 4; // Turn right
                    }
                    else
                    {
                        // Move forward
                        x = newX;
                        y = newY;

                        // Check if the new state was already visited
                        if (!visited.Add((x, y, direction)))
                        {
                            ret++;
                            break;
                        }
                    }
                }

                map[i][j] = '.';
            }
        }

        return ret;
    }
}