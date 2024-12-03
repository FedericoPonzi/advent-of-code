using System.Text.RegularExpressions;

namespace aoc2024;

public class Day3
{
    public static int solve2(String path)
    {
        const string pattern = @"don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\)";
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var corruptedMemory = File.ReadAllText(path);
        
        Regex regex = new Regex(pattern);

        // Track whether mul instructions are enabled
        bool mulEnabled = true;

        // Sum of all valid multiplications
        int sum = 0;

        // Find matches
        MatchCollection matches = regex.Matches(corruptedMemory);

        foreach (Match match in matches)
        {
            if (match.Value == "don't()")
            {
                mulEnabled = false;
            }
            else if (match.Value == "do()")
            {
                mulEnabled = true;
            }
            else if (match.Groups[1].Success && mulEnabled)
            {
                int x = int.Parse(match.Groups[1].Value);
                int y = int.Parse(match.Groups[2].Value);
                sum += x * y;
            }
        }

        return sum;
    }

    public static int solve1(String path)
    {
        const string pattern = @"mul\((\d{1,3}),(\d{1,3})\)";
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var corruptedMemory = File.ReadAllText(path);
        RegexOptions options = RegexOptions.ExplicitCapture;
        Regex regex = new Regex(pattern);
        MatchCollection matches = regex.Matches(corruptedMemory);
        return matches.Select(m => int.Parse(m.Groups[1].Value) * int.Parse(m.Groups[2].Value)).Sum();
    }
}