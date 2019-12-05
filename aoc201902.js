const prn = console.log.bind(console)
const int = (s) => parseInt(s, 10)

const input = '1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,1,23,13,27,2,6,27,31,1,5,31,35,2,10,35,39,1,6,39,43,1,13,43,47,2,47,6,51,1,51,5,55,1,55,6,59,2,59,10,63,1,63,6,67,2,67,10,71,1,71,9,75,2,75,10,79,1,79,5,83,2,10,83,87,1,87,6,91,2,9,91,95,1,95,5,99,1,5,99,103,1,103,10,107,1,9,107,111,1,6,111,115,1,115,5,119,1,10,119,123,2,6,123,127,2,127,6,131,1,131,2,135,1,10,135,0,99,2,0,14,0'
  .split(',')
  .map(int)

const run = (input, noun, verb) => {
  input = [...input]
  input[1] = noun
  input[2] = verb

  for (let i = 0; i < input.length; i += 4) {
    switch (input[i]) {
      case 99:
        return input[0]
      case 1:
        input[input[i + 3]] = input[input[i + 1]] + input[input[i + 2]]
        break
      case 2:
        input[input[i + 3]] = input[input[i + 1]] * input[input[i + 2]]
        break
    }
  }
}

const main1 = () => {
  prn(run(input, 12, 2))
}

const main2 = () => {
  for (let noun = 0; noun < 100; noun++)
    for (let verb = 0; verb < 100; verb++)
      if (run(input, noun, verb) == 19690720) {
        prn(noun, verb, 100 * noun + verb)
        return
      }
}

main1() // 2782414
main2() // 98 20 9820
