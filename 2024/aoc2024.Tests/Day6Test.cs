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
[TestSubject(typeof(Day6))]
public class Day6Test
{

    [TestMethod]
    public void testSolve1Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day6-example.txt");
        Assert.AreEqual(41, Day6.solve1(filePath));
    }
    
    [TestMethod]
    public void testSolve1()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day6-input.txt");
        Assert.AreEqual(4890, Day6.solve1(filePath));
    }
    [TestMethod]
    public void testSolve2Example()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day6-example.txt");
        Assert.AreEqual(6, Day6.solve2(filePath));
    }
    
    [TestMethod]
    public void testSolve2()
    {
        var projectDirectory = Directory.GetParent(AppContext.BaseDirectory).FullName;
        var filePath = Path.Combine(projectDirectory, "inputs", "day6-input.txt");
        Assert.AreEqual(1995, Day6.solve2(filePath));
    }
}