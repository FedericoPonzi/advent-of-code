import XCTest
@testable import day_16

final class day_16Tests: XCTestCase {
    func testSolution() throws {
        XCTAssertEqual(solve1("example.txt"), 46, "Example failed")
        //XCTAssertEqual(solve1("input.txt"), 7939, "Solve1 failed")
        XCTAssertEqual(solve2("example.txt"), 51, "Example p2 failed")
        //XCTAssertEqual(solve2("input.txt"), 8318, "Solve2 failed")
    }
}
