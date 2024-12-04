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
        var filePath = Path.Combine(projectDirectory, "inputs", "day4-example.txt");
        Assert.AreEqual(18, Day4.solve1(filePath));
    }
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day4-input.txt");
        Assert.AreEqual(2562, Day4.solve1(filePath));
    }
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day4-2-example.txt");
        Assert.AreEqual(9, Day4.solve2(filePath));
    }
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day4-input.txt");
        Assert.AreEqual(1902, Day4.solve2(filePath));
    }
}