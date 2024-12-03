using System;
using System.IO;
using aoc2024;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day3))]
public class Day3Test
{

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day3-example.txt");
        Assert.AreEqual(161, Day3.solve1(filePath));
    }
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day3-input.txt");
        Assert.AreEqual(168539636, Day3.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day3-2-example.txt");
        Assert.AreEqual(48, Day3.solve2(filePath));
    }
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day3-input.txt");
        Assert.AreEqual(97529391, Day3.solve2(filePath));
    }
}