const { int } = require('./lib')

const modes = {
  MEMORY: 0,
  IMMEDIATE: 1,
  RELATIVE: 2,
}

const io = {
  END: 'END',
  INPUT: 'INPUT',
  OUTPUT: 'OUTPUT',
}

const ioValues = new Set(Object.values(io))
const isIO = ({ value }) =>
  Array.isArray(value) && ioValues.has(value[0])

function parse(program) {
  return program.split(',').map(int)
}

const createWriteIO = () => {
  let value
  return {
    write: (v) => { value = v },
    read: () => value
  }
}

function runIntCode(program) {
  function* run(program, read) {
    yield ['IGNORE/INIT']

    let i = 0
    let base = 0

    const s = (p) => p || 0
    const p1 = (i) => i + 1
    const p2 = (i) => i + 2
    const p3 = (i) => i + 3

    const set = (i, value, mode) => {
      switch (mode) {
        case modes.RELATIVE: {
          program[s(program[i]) + base] = value
          break
        }
        default:
          program[s(program[i])] = value
      }
    }
    const get = (i, mode) => {
      switch (mode) {
        case modes.MEMORY: return s(program[s(program[i])])
        case modes.RELATIVE: return s(program[s(program[i]) + base])
        default: return s(program[i])
      }
    }

    for (; i < program.length;) {
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
          set(i3, get1() + get2(), p3Mode)
          i = i + 4
          break
        }
        case '02': {
          set(i3, get1() * get2(), p3Mode)
          i = i + 4
          break
        }
        case '03': {
          yield [io.INPUT]
          yield ['IGNORE/INPUT']
          set(i1, read(), p1Mode)
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
          set(i3, Number(isLess), p3Mode)
          i = i + 4
          break
        }
        case '08': {
          const isEqual = get1() == get2()
          set(i3, Number(isEqual), p3Mode)
          i = i + 4
          break
        }
        case '09': {
          base = base + get1()
          i = i + 2
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
  isIO,
  parse,
  runIntCode,
  runUntilDone: (gen, onOutput = () => {}) => {
    let current
    while (true) {
      current = gen.next()
      if (isIO(current) && current.value[0] == io.OUTPUT)
        onOutput(current.value[1])
      if (current.done) return current.value
    }
  },
  runUntilIO: (gen) => {
    let current
    while (true) {
      current = gen.next()
      if (isIO(current)) return current.value
      if (current.done) return
    }
  },
}
