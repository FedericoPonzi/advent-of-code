using System.Diagnostics;

namespace aoc2024;

public class Day7
{
    static List<string> GetPermutationsWithRepetition(string[] characters, int length)
    {
        if (length == 1)
        {
            return new List<string>(characters);
        }

        var shorterPermutations = GetPermutationsWithRepetition(characters, length - 1);
        var result = new List<string>();
        foreach (var shorter in shorterPermutations)
        {
            foreach (var character in characters)
            {
                result.Add(shorter + character);
            }
        }

        return result;
    }
    public static Int64 computeExpr(Int64[] input, char[] operators)
    {
        var res = input[0];
        for (var i = 1; i < input.Length; i++)
        {
            res = operators[i - 1] switch
            {
                '+' => res + input[i],
                '*' => res * input[i],
                '|' => Int64.Parse(string.Format($"{res}{input[i]}")),
            };
        }

        return res;
    }
    public static Int64 compute(String line, string[] operators)
    {
        var splitted = line.Split(": ");
        var expected = Int64.Parse(splitted[0]);
        var numbers = splitted[1].Split(" ").Select(Int64.Parse).ToArray();
        foreach(var comb in GetPermutationsWithRepetition(operators, numbers.Count()-1))
        {
            if (computeExpr(numbers, comb.ToCharArray()) == expected)
            {
                Console.WriteLine("found line: " + line);
                return expected;
            }
        }
        return 0;
    }
    public static Int64 solve1(string path)
    {
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path);
        return lines.AsParallel().Select(l => compute(l, new []{"*", "+"})).ToList().Sum();
    }

    public static Int64 solve2(string path)
    {
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path);
        return lines.AsParallel().Select(l => compute(l, new []{"*", "+", "|"})).ToList().Sum();
    }
}