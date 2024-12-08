using System.Diagnostics;

namespace aoc2024;

public class Day8
{
    public static int solve1(string path)
    {
        if (!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path);
        var freq = new Dictionary<char, List<(int, int)>>();
        var height = lines.Length;
        var width = lines[0].Length;
        for (var x = 0; x < width; x++)
        {
            for (var y = 0; y < height; y++)
            {
                var ch = lines[x][y];
                if (ch == '.') continue;
                var l = new List<(int, int)>();
                if (freq.ContainsKey(ch))
                {
                    l = freq[ch];
                }

                l.Add((x, y));
                freq[ch] = l;
            }
        }

        var antinodes = new HashSet<(int, int)>();
        foreach (var k in freq.Keys)
        {
            var pos = freq[k];
            for (var i = 0; i < pos.Count; i++)
            {
                for (var j = i + 1; j < pos.Count; j++)
                {
                    var (x1, y1) = pos[i];
                    var (x2, y2) = pos[j];
                    int mdX = Math.Abs(x2 - x1);
                    int mdY = Math.Abs(y2 - y1);
                    var mx1 = x1;
                    var mx2 = x2;
                    var my1 = y1;
                    var my2 = y2;
                    mx1 += x1 > x2 ? mdX : -mdX;
                    mx2 += x1 > x2 ? -mdX : mdX;
                    my1 += y1 > y2 ? mdY : -mdY;
                    my2 += y1 > y2 ? -mdY : mdY;

                    Func<(int, int), bool> isLegal = (xy) =>
                        xy.Item1 >= 0 && xy.Item1 < width && xy.Item2 >= 0 && xy.Item2 < height;

                    if (isLegal((mx1, my1)))
                    {
                        antinodes.Add((mx1, my1));
                    }

                    if (isLegal((mx2, my2)))
                    {
                        antinodes.Add((mx2, my2));
                    }
                }
            }
        }

        return antinodes.Count;
    }

    public static int solve2(string path)
    {
        if (!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path);
        var freq = new Dictionary<char, List<(int, int)>>();
        var height = lines.Length;
        var width = lines[0].Length;
        for (var x = 0; x < width; x++)
        {
            for (var y = 0; y < height; y++)
            {
                var ch = lines[x][y];
                if (ch == '.') continue;
                var l = new List<(int, int)>();
                if (freq.ContainsKey(ch))
                {
                    l = freq[ch];
                }

                l.Add((x, y));
                freq[ch] = l;
            }
        }

        var antinodes = new HashSet<(int, int)>();
        foreach (var k in freq.Keys)
        {
            var pos = freq[k];
            for (var i = 0; i < pos.Count; i++)
            {
                for (var j = i + 1; j < pos.Count; j++)
                {
                    var (x1, y1) = pos[i];
                    var (x2, y2) = pos[j];
                    if (pos.Count > 2)
                    {
                        antinodes.Add((x1, y1));
                        antinodes.Add((x2, y2));
                    }
                    int mdX = Math.Abs(x2 - x1);
                    int mdY = Math.Abs(y2 - y1);
                    var mx1 = x1;
                    var mx2 = x2;
                    var my1 = y1;
                    var my2 = y2;
                    Func<(int, int), bool> isLegal = (xy) =>
                        xy.Item1 >= 0 && xy.Item1 < width && xy.Item2 >= 0 && xy.Item2 < height;

                    while (isLegal((mx1, my1)) || isLegal((mx2, my2)))
                    {
                        mx1 += x1 > x2 ? mdX : -mdX;
                        mx2 += x1 > x2 ? -mdX : mdX;
                        my1 += y1 > y2 ? mdY : -mdY;
                        my2 += y1 > y2 ? -mdY : mdY;


                        if (isLegal((mx1, my1)))
                        {
                            antinodes.Add((mx1, my1));
                        }

                        if (isLegal((mx2, my2)))
                        {
                            antinodes.Add((mx2, my2));
                        }
                    }
                }
            }
        }

        foreach (var ant in antinodes)
        {
            Console.WriteLine("a: " + ant);
        }

        return antinodes.Count;
    }

}