object Day25 {

  private val start: InstructionId = 'A'
  private val steps: Int = 12459852

  private lazy val instructions: Map[InstructionId, Instruction] = List(
    Instruction(
      id = 'A',
      off = Rule(on = true, 1, 'B'),
      on = Rule(on = true, -1, 'E')
    ),
    Instruction(
      id = 'B',
      off = Rule(on = true, 1, 'C'),
      on = Rule(on = true, 1, 'F')
    ),
    Instruction(
      id = 'C',
      off = Rule(on = true, -1, 'D'),
      on = Rule(on = false, 1, 'B')
    ),
    Instruction(
      id = 'D',
      off = Rule(on = true, 1, 'E'),
      on = Rule(on = false, -1, 'C')
    ),
    Instruction(
      id = 'E',
      off = Rule(on = true, -1, 'A'),
      on = Rule(on = false, 1, 'D')
    ),
    Instruction(
      id = 'F',
      off = Rule(on = true, 1, 'A'),
      on = Rule(on = true, 1, 'C')
    )
  ).map(x => x.id -> x).toMap


  def one: Int = {
    val state = ((0 until steps) foldLeft State(start)) (step)

    state.tape.values count (on => on)
  }

  private def step(state: State, i: Int): State = {
    val instruction = instructions(state.instruction)
    val rule = if (state.tape.getOrElse(state.cursor, false)) {
      instruction.on
    } else {
      instruction.off
    }

    state.copy(
      rule.instruction,
      cursor = state.cursor + rule.move,
      tape = state.tape + (state.cursor -> rule.on)
    )
  }

  type InstructionId = Char

  case class Instruction(id: InstructionId, off: Rule, on: Rule)

  case class Rule(on: Boolean, move: Int, instruction: InstructionId)

  case class State(instruction: InstructionId, cursor: Int = 0, tape: Map[Int, Boolean] = Map())

}
