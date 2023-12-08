const [solve1, solve2] = require('./main');

test('part 1', () => {
  expect(solve1('example.txt')).toBe(2);
  expect(solve1('example2.txt')).toBe(6);
  expect(solve1('input.txt')).toBe(18827);
});

test('part 2', () => {
  expect(solve2('example.txt')).toBe(2);
  expect(solve2('example2.txt')).toBe(6);
  expect(solve2('input.txt')).toBe(20220305520997);
});
