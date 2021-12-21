let example = [
  // [-1, 0, 2, 0, 0, 0],
  // [2, -10, -7, 0, 0, 0],
  // [4, -8, 8, 0, 0, 0],
  // [3, 5, -1, 0, 0, 0],
  [-4, -9, -3, 0, 0, 0],
  [-13, -11, 0, 0, 0, 0],
  [-17, -7, 15, 0, 0, 0],
  [-16, 4, 2, 0, 0, 0],
]

const pairs = [
  [2, 3], [1, 3], [0, 3], [0, 2], [1, 2], [0, 1]
]

const copy = (rows) => rows.map(row => row.map((x) => x))

const updateVelocity = (inputs) => {
  pairs.forEach(([a, b]) => {
    const ia = inputs[a]
    const ib = inputs[b]

    for (let i = 0; i < 3; i++) {
      if (ia[i] < ib[i]) {
        ia[i + 3]++
        ib[i + 3]--
      }
      if (ia[i] > ib[i]) {
        ia[i + 3]--
        ib[i + 3]++
      }
    }
  })
}

const updatePosition = (inputs) => {
  for (let i = 0; i < inputs.length; i++)
    for (let j = 0; j < 3; j++)
      inputs[i][j] = inputs[i][j] + inputs[i][j + 3]
}

const plus = (a, b) => a + b

const main1 = () => {
  let inputs = copy(example)

  for (let i = 0; i < 10; i++) {
    updateVelocity(inputs)
    updatePosition(inputs)
  }

  const energy = (input) => {
    let xs = 0
    let ys = 0
    for (let i = 0; i < 3; i++) {
      xs += Math.abs(input[i])
      ys += Math.abs(input[i + 3])
    }
    return xs * ys
  }

  console.log(inputs.map(energy).reduce(plus))
}

const gcm = (a, b) => a ? gcm(b % a, a) : b
const lcm = (a, b) => a * b / gcm(a, b)

const main2 = () => {
  const init = copy(example)
  let inputs = copy(example)

  const compare = (a, b, d) => {
    for (let i = 0; i < a.length; i++)
      if (a[i][0 + d] != b[i][0 + d] || a[i][3 + d] != b[i][3 + d])
        return false
    return true
  }

  const x = 0
  const y = 1
  const z = 2

  let i = 1
  let result = [0, 0, 0]
  while (result[x] == 0 || result[y] == 0 || result[z] == 0) {
    updateVelocity(inputs)
    updatePosition(inputs)
    if (result[x] == 0 && compare(init, inputs, x)) result[x] = i
    if (result[y] == 0 && compare(init, inputs, y)) result[y] = i
    if (result[z] == 0 && compare(init, inputs, z)) result[z] = i
    i++
  }

  console.log(result.reduce(lcm))
}

main1()
main2()
