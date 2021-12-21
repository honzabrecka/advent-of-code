const fs = require('fs')
const readline = require('readline')

const pipe = (...fns) => (init) => fns.reduce((v, f) => f(v), init)

const then = (f) => (x) => x.then(f)

const reduce = (reducer, init) => (seq) => {
  if (typeof seq[Symbol.iterator] === 'function')
    return (() => {
      let result = init
      for (const x of seq)
        result = reducer(result, x)
      return result
    })()
  if (typeof seq[Symbol.asyncIterator] === 'function')
    return (async () => {
      let result = init
      for await (const x of seq)
        result = reducer(result, x) // TODO await reducer?
      return result
    })()
  throw new Error('Not implemented for seq', seq)
}

const map = (transformer) => (seq) => {
  if (typeof seq[Symbol.iterator] === 'function')
    return (function* () {
      for (const x of seq)
        yield transformer(x)
    })()
  if (typeof seq[Symbol.asyncIterator] === 'function')
    return (async function* () {
      for await (const x of seq)
        yield transformer(x) // TODO await transformer?
    })()
  throw new Error('Not implemented for seq', seq)
}

const sort = (compare) => (seq) => {
  if (Array.isArray(seq))
    return (function () {
      return seq.sort(compare)
    })()
  throw new Error('Not implemented for seq', seq)
}

// String -> AsyncIterator
const lineReader = (file) => readline.createInterface({
  input: fs.createReadStream(file),
  crlfDelay: Infinity
})

const prn = console.log.bind(console)

///

const aocInputReader = (file) => lineReader(`${__dirname}/${file}`)

module.exports = {
  pipe,
  then,
  reduce,
  map,
  sort,
  ///
  lineReader,
  prn,
  ///
  int: (s) => parseInt(s, 10),
  aocInputReader,
}
