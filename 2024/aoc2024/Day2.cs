namespace day_2;

public class Day2
{
    public static bool isSafeReport(int[] report)
    {
        bool increasing = true, decreasing = true;
        
        for (int i = 1; i < report.Length; i++)
        {
            int diff = report[i] - report[i - 1];
            if (diff > 0) decreasing = false;
            if (diff < 0) increasing = false;
            diff = Math.Abs(diff);
            if (diff < 1 || diff > 3)
                return false;
        }
        
        return increasing || decreasing;
    }

    public static bool testSafe(int[] report)
    {
        if (isSafeReport(report)) return true;
        for (int i = 0; i < report.Length; i++)
        {
            var reducedReport = report.Where((_, index) => index != i).ToArray();
            if (isSafeReport(reducedReport))
                return true;
        }
        return false;
    }
    public static int solve1(String path)
    {
        return File.ReadAllLines(path)
            .Select(l => l.Split(' ').Select(int.Parse).ToArray())
            .Select(isSafeReport)
            .Where(b => b)
            .ToList()
            .Count();
    }

    public static int solve2(String path)
    {
        return File.ReadAllLines(path)
            .Select(l => l.Split(' ').Select(int.Parse).ToArray())
            .Select(testSafe)
            .Where(b => b)
            .ToList()
            .Count();
    }
}