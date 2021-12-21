const { prn, pipe, reduce, sort } = require('./lib')
const { parse, runIntCode, io, runUntilIO } = require('./intCodeComputer')

const program = parse('3,8,1001,8,10,8,105,1,0,0,21,42,59,76,85,106,187,268,349,430,99999,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,102,3,9,9,101,3,9,9,1002,9,2,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,4,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99')

function* permutations5(from, to) {
  for (let a = from; a < to; a++)
    for (let b = from; b < to; b++)
      for (let c = from; c < to; c++)
        for (let d = from; d < to; d++)
          for (let e = from; e < to; e++) {
            const p = [a, b, c, d, e]
            if (new Set(p).size == 5) yield p
          }
}

const createGen = (program) => (phase) => {
  const [gen, write] = runIntCode(program)
  let phaseSet = false
  let output
  return (value) => {
    if (!phaseSet) {
      runUntilIO(gen)
      write(phase)
      phaseSet = true
    }
    const next = runUntilIO(gen)
    if (next[0] == io.END)
      return [io.END, output]
    else
      write(value)
    output = runUntilIO(gen)[1]
    return [io.OUTPUT, output]
  }
}

const runGens = (gens, value) => {
  const result = gens.reduce((x, gen) => gen(x[1]), [undefined, value])
  return result[0] == io.END ? value : runGens(gens, result[1])
}

const main1 = (program) =>
  pipe(
    reduce((res, seq) => {
      const gens = seq.map(createGen(program))
      res.push([seq, runGens(gens, 0)])
      return res
    }, []),
    sort((a, b) => b[1] - a[1]),
    ([first]) => first,
    prn,
  )(permutations5(0, 5))

const main2 = (program) =>
  pipe(
    reduce((res, seq) => {
      const gens = seq.map(createGen(program))
      res.push([seq, runGens(gens, 0)])
      return res
    }, []),
    sort((a, b) => b[1] - a[1]),
    ([first]) => first,
    prn,
  )(permutations5(5, 10))

main1(program) // 34686 for permutation 42103
main2(program) // 36384144 for permutation 76589
