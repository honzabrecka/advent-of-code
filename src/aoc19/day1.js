const { prn, pipe, then, aocInputReader, reduce, map, int } = require('./lib')

const fuel = pipe(
  (n) => n / 3,
  Math.floor,
  (n) => n - 2,
)

const main1 = () =>
  pipe(
    map(int),
    map(fuel),
    reduce((a, b) => a + b, 0),
    then(prn),
  )(aocInputReader('../../data/aoc19/day1.txt'))

const recursiveFuel = (n) => {
  const m = fuel(n)
  return m <= 0 ? n : n + recursiveFuel(m)
}

const main2 = () =>
  pipe(
    map(int),
    map(pipe(fuel, recursiveFuel)),
    reduce((a, b) => a + b, 0),
    then(prn),
  )(aocInputReader('../../data/aoc19/day1.txt'))

main1() // 3297866
main2() // 4943923
