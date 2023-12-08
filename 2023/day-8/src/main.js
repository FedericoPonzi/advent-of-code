const fs = require("fs");


function parse(filename) {
    let input = fs.readFileSync(filename, 'utf8').split('\n');
    let instructions = input[0].split("").map((c) => c === "L"? 0 : 1);
    let ret = {};
    // iterate on the rest of the input, break the line "aa = (ab, bc)"
    // and create a new node(aa, ab, bc)
    for (let i = 2; i < input.length; i++) {
        let [name, value] = input[i].split(' = ');
        let [left, right] = value.split(', ');
        ret[name] = [left.substring(1), right.substring(0, right.length-1)]
    }
    return [instructions, ret];
}
const INITIAL_NODE = "AAA";
const FINAL_NODE = "ZZZ";

function visit(instructions, nodes) {
    let next = INITIAL_NODE;
    let index = 0;
    while (next !== FINAL_NODE) {
        next = nodes[next][instructions[index%instructions.length]];
        index = index + 1; 
    }
    return index;
}

function solve1(filename) {
    let content = parse(filename);
    return visit(content[0], content[1]);
}

module.exports = solve1;