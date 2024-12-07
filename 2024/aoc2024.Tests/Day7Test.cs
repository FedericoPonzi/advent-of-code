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
[TestSubject(typeof(Day7))]
public class Day7Test
{
    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day7-example.txt");
        Assert.AreEqual(3749, Day7.solve1(filePath));
    }

    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day7-input.txt");
        Assert.AreEqual(2664460013123, Day7.solve1(filePath));
    }

    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day7-example.txt");
        Assert.AreEqual(11387, Day7.solve2(filePath));
    }

    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day7-input.txt");
        Assert.AreEqual(426214131924213, Day7.solve2(filePath));
    }

    [TestMethod]
    public void testCompute()
    {
        var val = "190: 10 19";
        Assert.AreEqual(190, Day7.compute(val, new []{"*", "+"}));
    }

    [TestMethod]
    public void testComputeExpr()
    {
        Assert.AreEqual(29, Day7.computeExpr(new long[] { 10, 19 }, new[] { '+' }));
    }
}