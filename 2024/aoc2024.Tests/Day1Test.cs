using System;
using System.IO;
using day_1;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day1))]
public class Day1Test
{

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day1-example.txt");
        Assert.AreEqual(11, Day1.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day1-input.txt");
        Assert.AreEqual(2196996, Day1.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day1-example.txt");
        Assert.AreEqual(31, Day1.solve2(filePath));
    }
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day1-input.txt");
        Assert.AreEqual(23655822, Day1.solve2(filePath));
    }

}