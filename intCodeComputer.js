const { int } = require('./lib')

const modes = {
  MEMORY: 0,
  IMMEDIATE: 1,
}

function parse(program) {
  return program.split(',').map(int)
}

function* runIntCode(program) {
  program = [...program]

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
        return get(0, modes.IMMEDIATE)
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
        set(i1, yield)
        i = i + 2
        break
      }
      case '04': {
        yield get1()
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

module.exports = {
  modes,
  parse,
  runIntCode,
}
