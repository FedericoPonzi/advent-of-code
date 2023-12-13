def solve(filename)
  res = 0
  File.open(filename, "r").each_line do |line|
    ret = 0
    line, expected_springs = line.split(" ")
    expected_springs = expected_springs.split(",").map(&:to_i)

    wildcards = (0...line.length).find_all { |i| line[i, 1] == '?' }

    (0..2 ** wildcards.length - 1).map { |i| "%0#{wildcards.length}b" % i }.each { |bin_mask|
      line_clone = line.split("").dup
      bin_mask.chars.zip(wildcards).each { |mask|
        if mask[0] == "1"
          line_clone[mask[1]] = "#"
        else
          line_clone[mask[1]] = "."
        end
      }
      if line_clone.join.scan(/(#+)+/).flatten.map(&:length) == expected_springs
        ret += 1
      end
    }
    res += ret
  end
  res
end

if __FILE__ == $0
  p ["received", solve("example.txt"), "expected", 21]
  # p ["received", solve("input.txt"), "expected", 6935]
end