# 2024
For this year, I'm planning to use Advent Of Code to learn more about .NET, so mostly C# or F#.

As always, if you have feedback of suggestion please do reach out!

--
Template:
```c#
namespace aoc2024;

public class Day
{
    public static int solve1(String path)
    {
        if(!Path.Exists(path)) throw new FileNotFoundException();
        var lines = File.ReadAllLines(path);
    }
}

```
```c#
using System;
using System.IO;
using aoc2024;
using day_2;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day4))]
public class Day4Test
{

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day2-example.txt");
        Assert.AreEqual(4, Day2.solve2(filePath));
    }
}
```