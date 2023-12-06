datatype Pair = Tuple(0: int, 1: int)

method {:verify false} count(race_time: int, record: int) returns (r: int) {
    for x := 0 to race_time {
        if ((race_time - x) * x > record) {
            r := r + 1;
        }
    }
}

method {:verify false} solve1(input: array<Pair>) returns (r: int) {
    r:= 1;
    for i:= 0 to input.Length {       
        var p := input[i];
        var c := count(p.0, p.1);
        r := r * c;
    }
}

method CreateExampleInput() returns (pairs: array<Pair>)
{
    pairs := new Pair[3];

    pairs[0] := Tuple(7, 9);
    pairs[1] := Tuple(15, 40);
    pairs[2] := Tuple(30, 200);
}

method CreateExampleInput2() returns (pairs: array<Pair>)
{
    pairs := new Pair[1];
    pairs[0] := Tuple(71530, 940200);
}
method CreateInput() returns (pairs: array<Pair>)
{
    pairs := new Pair[4];
    pairs[0] := Tuple(53, 250);
    pairs[1] := Tuple(91, 1330);
    pairs[2] := Tuple(67, 1081);
    pairs[3] := Tuple(68, 1025);
}

method CreateInput2() returns (pairs: array<Pair>)
{
    pairs := new Pair[1];
    pairs[0] := Tuple(53916768, 250133010811025);
}

method Main() {
    var example:= CreateExampleInput();
    var res := solve1(example); // 288
    print "Solve1 example: ", res, "\n";

    var input:= CreateInput();
    res := solve1(input); // 625968
    print "Solve1 solution: ", res, "\n";

    example:= CreateExampleInput2();
    res := solve1(example); // 71503
    print "Solve2 example: ", res, "\n";

    input:= CreateInput2();
    res := solve1(input); // 43663323
    print "Solve2 solution: ", res, "\n";

}