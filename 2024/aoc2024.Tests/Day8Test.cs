using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using aoc2024;
using day_2;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace aoc2024.Tests;

[TestClass]
[TestSubject(typeof(Day8))]
public class Day8Test
{
    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day8-example.txt");
        Assert.AreEqual(14, Day8.solve1(filePath));
    }

    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day8-input.txt");
        Assert.AreEqual(293, Day8.solve1(filePath));
    }

    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day8-example.txt");
        Assert.AreEqual(34, Day8.solve2(filePath));
    }

    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day8-input.txt");
        Assert.AreEqual(934, Day8.solve2(filePath));
    }
}