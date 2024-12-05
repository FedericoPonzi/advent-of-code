using System;
using System.IO;
using aoc2024;
using day_2;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day5))]
public class Day5Test
{

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day5-example.txt");
        Assert.AreEqual(143, Day5.solve1(filePath));
    }
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day5-input.txt");
        Assert.AreEqual(5948, Day5.solve1(filePath));
    }
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day5-example.txt");
        Assert.AreEqual(123, Day5.solve2(filePath));
    }
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day5-input.txt");
        Assert.AreEqual(3062, Day5.solve2(filePath));
    }
}