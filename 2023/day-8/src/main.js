const fs = require("fs");

function gcd(a, b) {
    return b === 0 ? a : gcd(b, a % b);
}
function findLCM(arr) {
    let result = arr[0];
    for (let i = 1; i < arr.length; i++) {
        result = (result * arr[i]) / gcd(result, arr[i]);
    }
    return result;
}

function parse(filename) {
    let input = fs.readFileSync(filename, 'utf8').split('\n');
    let instructions = input[0].split("").map((c) => c === "L"? 0 : 1);
    let ret = new Map();
    for (let i = 2; i < input.length; i++) {
        let [name, value] = input[i].split(' = ');
        let [left, right] = value.split(', ');
        ret.set(name, [left.substring(1), right.substring(0, right.length-1)])
    }
    return [instructions, ret];
}

function visit(instructions, nodes, start_node, end_node) {
    let next = start_node;
    let index = 0;
    while (!next.endsWith(end_node)) {
        next = nodes.get(next)[instructions[index%instructions.length]];
        index = index + 1; 
    }
    return index;
}


function solve2(filename) {
    let content = parse(filename);
    var paths_length = [...content[1].keys()]
        .filter(k => k.endsWith("A"))
        .map(start_node => visit(content[0], content[1], start_node, "Z"))
    return findLCM(paths_length);
}


function solve1(filename) {
    let content = parse(filename);
    return visit(content[0], content[1], "AAA", "ZZZ");
}


module.exports = [solve1, solve2];
