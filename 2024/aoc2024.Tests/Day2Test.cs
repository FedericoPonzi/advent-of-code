using System;
using System.IO;
using System.Linq;
using day_2;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day2))]
public class Day2Test
{
    [TestMethod]
    public void testIsSafeReport()
    {
        Assert.IsTrue(Day2.isSafeReport("7 6 4 2 1".Split(" ").Select(int.Parse).ToArray()));
        Assert.IsTrue(Day2.isSafeReport("1 3 6 7 9".Split(" ").Select(int.Parse).ToArray()));
        Assert.IsFalse(Day2.isSafeReport("1 3 2 4 5".Split(" ").Select(int.Parse).ToArray()));
    }

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day2-example.txt");
        Assert.AreEqual(2, Day2.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day2-input.txt");
        Assert.AreEqual(359, Day2.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day2-example.txt");
        Assert.AreEqual(4, Day2.solve2(filePath));
    }
    
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day2-input.txt");
        Assert.AreEqual(418, Day2.solve2(filePath));
    }
}