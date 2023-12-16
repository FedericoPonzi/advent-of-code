import Foundation

extension String {
    public subscript(_ idx: Int) -> Character {
        self[self.index(self.startIndex, offsetBy: idx)]
    }
}
func mod(_ a: Int, _ n: Int) -> Int {
    precondition(n > 0, "modulus must be positive")
    let r = a % n
    return r >= 0 ? r : r + n
}

func parse(_ filename: String) -> [String] {
    let fileURL = URL(fileURLWithPath: filename)
    let content = try! String(contentsOf: fileURL)
    let lines = content.components(separatedBy: .newlines)
    let nonEmptyLines = lines.filter { !$0.isEmpty }
    return nonEmptyLines
}

func next_row_col(_ current: (Int, Int), _ direction: Int) -> (Int, Int) {
    let (dx,dy) = [(0,+1),(1,0), (0,-1),(-1,0)][direction]
    return (current.0+dx, current.1 + dy)
}
let NORTH = 3
let EAST = 0
let SOUTH = 1
let WEST = 2
func translate(_ i: Int) -> String{
    return ["east","South","West","north"][i]
}

func traverseMaze(_ startAt: (Int, Int), _ direction: Int, _ inputMap: [String], _ energized: inout Set<[Int]>) {
    var direction = direction
    var startAt = startAt

    while startAt.0 >= 0 && startAt.0 < inputMap.count && startAt.1 >= 0 && startAt.1 < inputMap[0].count {
        let (row, col) = startAt
        if(energized.contains([row, col, direction])) {
            return;
        }
        energized.insert([row, col, direction])
        let ch = inputMap[row][col]
        if(ch == "|" && (direction == EAST || direction == WEST)) {
            traverseMaze(next_row_col(startAt, NORTH), NORTH, inputMap, &energized)
            direction = SOUTH
        }
        if(ch == "-" && (direction == NORTH || direction == SOUTH)) {
            traverseMaze(next_row_col(startAt, WEST), WEST, inputMap, &energized)
            direction = EAST
        }
        if(ch == "/" || ch == "\\" ) {
            // last option:  a 90 degree turn
            let left_turn_1 = ch == "/" && (direction == WEST || direction == EAST)
            let left_turn_2 = ch == "\\" && (direction == SOUTH || direction == NORTH)
            direction = mod(direction + (left_turn_1 || left_turn_2 ? -1 : 1), 4)
        }
        startAt = next_row_col(startAt, direction)

    }
    fflush(stdout)
}

func solve1(_ filename: String) -> Int {
    let inputMap = parse(filename)
    var visited = Set<[Int]>()

    traverseMaze((0,0), 0, inputMap, &visited)
    var res = Set<[Int]>()
    for el in visited {
        res.insert([el[0], el[1]])
    }
    return res.count
}
func get_res(_ a: Set<[Int]>) -> Int{
   var res = Set<[Int]>()
   for el in a {
       res.insert([el[0], el[1]])
   }
   return res.count
}
func solve2(_ filename: String) -> Int {
   let inputMap = parse(filename)
   fflush(stdout)
   let cols = inputMap[0].count;
   let rows = inputMap.count
   var max_config_count = 0
   var visited = Set<[Int]>()
   for col in 0...cols {
      // TOP row heading downward
      visited = Set<[Int]>()
      traverseMaze((0,col), SOUTH, inputMap, &visited)
      max_config_count = max(get_res(visited), max_config_count)
      // bottom row, heading upward
      visited = Set<[Int]>()
      traverseMaze((rows,col), NORTH, inputMap, &visited)
      max_config_count = max(get_res(visited), max_config_count)
   }

   for row in 0...rows {
      visited = Set<[Int]>()
      traverseMaze((row,0), EAST, inputMap, &visited)
      max_config_count = max(get_res(visited), max_config_count)
      // bottom row, heading upward
      visited = Set<[Int]>()
      traverseMaze((row,cols), WEST, inputMap, &visited)
      max_config_count = max(get_res(visited), max_config_count)
   }
   return max_config_count
}
