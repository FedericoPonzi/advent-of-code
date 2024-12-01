namespace day_1;

public class Day1
{
    public static int solve1(String p)
    {
        if (!Path.Exists(p))
        {
            throw new Exception($"Path {p} doesn't exists");
        }
        var input = File.ReadAllLines(p);
        List<int> left = new List<int>();
        List<int> right = new List<int>();
        foreach (var line in input)
        {
            var splitted = line.Split("   ");
            left.Add(int.Parse(splitted[0]));
            right.Add(int.Parse(splitted[1]));
        }
        left.Sort();
        right.Sort();
        return left.Zip(right, (a, b) => Math.Abs(a - b)).Sum();
    }

    public static int solve2(String p)
    {
        if (!Path.Exists(p))
        {
            throw new Exception($"Path {p} doesn't exists");
        }

        var input = File.ReadAllLines(p);
        List<int> left = new List<int>();
        List<int> right = new List<int>();
        foreach (var line in input)
        {
            var splitted = line.Split("   ");
            left.Add(int.Parse(splitted[0]));
            right.Add(int.Parse(splitted[1]));
        }

        var instancesCount = right.GroupBy(n => n)
            .ToDictionary(group => group.Key, group => group.Count());

        return left.Select(i => i * instancesCount.GetValueOrDefault(i, 0)).Sum();
        
    }
}
