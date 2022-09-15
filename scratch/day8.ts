const two_diffs = [
  [0, 2],
  [1, 1],
  [1, 2],
  [1, 2],
  [1, 2],
  [2, 2],
  [3, 1],
  [3, 2],
  [4, 1],
];

const three_diffs = [
  [0, 1],
  [0, 2],
  [1, 1],
  [1, 1],
  [1, 2],
  [1, 2],
  [2, 0],
  [2, 1],
  [3, 1],
];

const five_diffs = [
  [0, 1],
  [0, 1],
  [0, 2],
  [1, 1],
  [1, 2],
  [2, 1],
  [2, 2],
  [3, 1],
  [4, 1],
];

const six_diffs = [
  [1, 1],
  [5, 1],
  [2, 1],
  [2, 1],
  [3, 1],
  [1, 0],
  [4, 1],
  [0, 1],
  [1, 1],
];

const nine_diffs = [
  [1, 1],
  [4, 0],
  [3, 1],
  [1, 0],
  [2, 0],
  [1, 0],
  [1, 1],
  [3, 0],
  [0, 1],
];

const zero_diffs = [
  [4, 0],
  [2, 1],
  [2, 1],
  [2, 1],
  [2, 1],
  [1, 1],
  [3, 0],
  [0, 1],
  [1, 1],
];

const all_diffs: Array<[number, Array<any>]> = [
  [2, two_diffs],
  [3, three_diffs],
  [5, five_diffs],
  [6, six_diffs],
  [9, nine_diffs],
  [0, zero_diffs],
];

const combinations = <T>(elements: Array<T>): Array<Array<T>> => {
  if (elements.length == 1) {
    return [elements];
  } else {
    const tail = combinations(elements.slice(1));
    return tail.reduce(
      (combos, combo) => {
        combos.push([elements[0], ...combo]);
        return combos;
      },
      [[elements[0]], ...tail],
    );
  }
};

const uniques = new Set();

// all_diffs.map(([x, diffs]) => {
//   const y = diffs.reduce(([x1, y1], [x2, y2]) => [x1 + x2, y1 + y2]);
//   console.log(x, y[0] + y[1]);
// });
//
function sum(xs: Array<number>): number {
  return xs.reduce((acc, x) => acc + x);
}

const zero = "abcdef";
const one = "bc";
const two = "abdeg";
const three = "abcdg";
const four = "bcfg";
const five = "acdfg";
const six = "acdefg";
const seven = "abc";
const eight = "abcdefg";
const nine = "abcdfg";

const numbers = [zero, one, two, three, four, five, six, seven, eight, nine];

const same = (A: string) => {
  return numbers.filter((B) => B != A).map((B) => {
    let x = 0;
    for (const char of B) {
      if (A.includes(char)) {
        x += 1;
      }
    }
    return x;
  });
};
for (let i = 0; i < numbers.length; i++) {
  console.log(`(${sum(same(numbers[i]))}, ${i}),`);
}
