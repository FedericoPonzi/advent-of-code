namespace aoc2024;

public class Day5
{
    public static bool hasRightOrder(List<(int,int)> rules, List<int> pages)
    {
        var filtered = rules.Where(rule => pages.Contains(rule.Item1) && pages.Contains(rule.Item2)).ToList();
        foreach (var (l, r) in filtered)
        {
            if (pages.IndexOf(l) > pages.IndexOf(r))
            {
                return false;
            }
        }
        return true;
    }
    public static int solve1(String path)
    {
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllText(path);
        var content = lines.Split("\n\n");
        var rules = content[0].Split('\n').Select(rule => rule.Split('|').Select(int.Parse).ToArray())
            .Select(pair => (pair[0], pair[1]))
            .ToList();
        var res = 0;
        foreach (var pagesLine in content[1].Split('\n'))
        {
            var pages = pagesLine.Split(',').Select(int.Parse).ToList();
            if (!hasRightOrder(rules, pages))
            {
                continue;
            }
            int middlePage = pages.Count / 2;
            res += pages[middlePage];
        }
        
        return res;
    }
    public static bool verifyOrFixOrder(List<(int,int)> rules, List<int> pages, bool updated = false)
    {
        foreach (var (l, r) in rules)
        {
            var indexL = pages.IndexOf(l);
            var indexR = pages.IndexOf(r);
            if (indexL > indexR)
            {
                pages.Insert(indexL, r);
                pages.Insert(indexR, l);
                return verifyOrFixOrder(rules, pages, true);
            }
        }
        return updated;
    }

    public static List<int> TopologicalSort(List<int> pages, List<(int, int)> rules)
    {
        var graph = new Dictionary<int, List<int>>();
        var inDegree = new Dictionary<int, int>();
        foreach (var page in pages)
        {
            graph[page] = new List<int>();
            inDegree[page] = 0;
        }

        foreach (var (l, r) in rules)
        {
            graph[l].Add(r);
            inDegree[r]++;
        }
        
        // start by inserting pages with no precedence rule
        var toVisit = new Queue<int>(pages.Where(page => inDegree[page] == 0));
        var sorted = new List<int>();

        while (toVisit.Count > 0)
        {
            var current = toVisit.Dequeue();
            sorted.Add(current);

            foreach (var neighbor in graph[current])
            {
                inDegree[neighbor]--;
                if (inDegree[neighbor] == 0)
                    toVisit.Enqueue(neighbor);
            }
        }
        return sorted;
    }
    public static int solve2(String path)
    {
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllText(path);
        var content = lines.Split("\n\n");
        var rules = content[0].Split('\n').Select(rule => rule.Split('|').Select(int.Parse).ToArray())
            .Select(pair => (pair[0], pair[1]))
            .ToList();
        var res = 0;
        foreach (var pagesLine in content[1].Split('\n'))
        {
            var pages = pagesLine.Split(',').Select(int.Parse).ToList();
            var filtered = rules.Where(rule => pages.Contains(rule.Item1) && pages.Contains(rule.Item2)).ToList();
            if (hasRightOrder(filtered, pages))
            {
                continue;
            }
            // Reorder the update using topological sort
            pages = TopologicalSort(pages, filtered);
            int middlePage = pages.Count / 2;
            res += pages[middlePage];
        }
        
        return res;
    }

}