const { int } = require('./lib')

const modes = {
  MEMORY: 0,
  IMMEDIATE: 1,
}

const io = {
  END: 'END',
  INPUT: 'INPUT',
  OUTPUT: 'OUTPUT',
}

function parse(program) {
  return program.split(',').map(int)
}

const createWriteIO = () => {
  let value
  return {
    write: (v) => { value = v },
    read: () => value
  }
}

function runIntCode(program) {
  function* run(program, read) {
    yield ['IGNORE/INIT']

    const p1 = (i) => i + 1
    const p2 = (i) => i + 2
    const p3 = (i) => i + 3

    const set = (i, value) => program[program[i]] = value
    const get = (i, mode) => mode === modes.MEMORY
      ? program[program[i]]
      : program[i]

    for (let i = 0; i < program.length;) {
      const i1 = i + 1
      const i2 = i + 2
      const i3 = i + 3

      const [
        op2, op1 = 0,
        p1Mode = modes.MEMORY,
        p2Mode = modes.MEMORY,
        p3Mode = modes.MEMORY
      ] = program[i]
        .toString(10)
        .split('')
        .map(int)
        .reverse()
      const opCode = `${op1}${op2}`

      const get1 = () => get(i1, p1Mode)
      const get2 = () => get(i2, p2Mode)

      switch (opCode) {
        case '99':
          return [io.END, get(0, modes.IMMEDIATE)]
        case '01': {
          set(i3, get1() + get2())
          i = i + 4
          break
        }
        case '02': {
          set(i3, get1() * get2())
          i = i + 4
          break
        }
        case '03': {
          yield [io.INPUT]
          yield ['IGNORE/INPUT']
          set(i1, read())
          i = i + 2
          break
        }
        case '04': {
          yield [io.OUTPUT, get1()]
          i = i + 2
          break
        }
        case '05': {
          if (get1() != 0)
            i = get2()
          else
            i = i + 3
          break
        }
        case '06': {
          if (get1() == 0)
            i = get2()
          else
            i = i + 3
          break
        }
        case '07': {
          const isLess = get1() < get2()
          set(i3, Number(isLess))
          i = i + 4
          break
        }
        case '08': {
          const isEqual = get1() == get2()
          set(i3, Number(isEqual))
          i = i + 4
          break
        }
        default:
          i = i + 1
      }
    }
  }

  const { read, write } = createWriteIO()
  const gen = run([...program], read)
  // gen.next()
  return [
    gen,
    (input) => {
      write(input)
      gen.next()
    }
  ]
}

module.exports = {
  modes,
  io,
  parse,
  runIntCode,
  runUntilDone: (gen) => {
    while (!(current = gen.next()).done) {}
    return current.value
  },
  runUntilIO: (gen) => {
    const IOValues = new Set(Object.values(io))
    const isIO = ({ value }) =>
      Array.isArray(value) && IOValues.has(value[0])
    while (true) {
      const current = gen.next()
      if (isIO(current)) return current.value
      if (current.done) return
    }
  }
}
