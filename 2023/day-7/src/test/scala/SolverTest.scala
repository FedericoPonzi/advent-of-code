
class SolverTest extends munit.FunSuite:
  test("example part1") {
    val expected = 6440
    val received = solve1(os.pwd/"example.txt")
    assertEquals(received, expected)
  }
  test("part1 getValue") {
    assertEquals(getKind("AAAAA".split("")), 7)
    assertEquals(getKind("AA8AA".split("")), 6)
    assertEquals(getKind("23332".split("")), 5)
    assertEquals(getKind("TTT98".split("")), 4)
    assertEquals(getKind("23432".split("")), 3)
    assertEquals(getKind("A23A4".split("")), 2)
    assertEquals(getKind("23456".split("")), 1)
  }
//  test("input part1") {
//    val expected = 254024898
//    val received = solve1(os.pwd / "input.txt")
//    assertEquals(received, expected)
//  }
  test("part2 getValue2") {
    assertEquals(getKind2("32T3K".split("")), getKind("32T3K".split("")))
    // full house:
    assertEquals(getKind2("JJJJJ".split("")), 7)
    assertEquals(getKind2("JJJJA".split("")), 7)
    assertEquals(getKind2("JJJAA".split("")), 7)
    assertEquals(getKind2("JJAAA".split("")), 7)
    assertEquals(getKind2("JAAAA".split("")), 7)
    // four of a kind:
    assertEquals(getKind2("JAAAB".split("")), 6)
    assertEquals(getKind2("JJAAB".split("")), 6)
    assertEquals(getKind2("JJAAB".split("")), 6)
    assertEquals(getKind2("JJJAB".split("")), 6)
    // three of a kind:
    assertEquals(getKind2("JJABC".split("")), 4) // three of a kind
    assertEquals(getKind2("JAABC".split("")), 4) // three of a kind
    // two pairs:impossible with jokers
    assertEquals(getKind2("AABBC".split("")), 3)
    // one pair
    assertEquals(getKind2("A23A4".split("")), 2)
    assertEquals(getKind2("JABCD".split("")), 2)

  }
  test("example part2") {
    val expected = 5905
    val received = solve2(os.pwd / "example.txt")
    assertEquals(received, expected)
  }
  test("input part2") {
    val expected = 254115617
    val received = solve2(os.pwd / "input.txt")
    assertEquals(received, expected)
  }


