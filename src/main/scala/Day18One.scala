import scala.annotation.tailrec
import scala.io.Source

object Day18One {
  def input: Iterator[String] = Source.fromResource("day18.txt").getLines

  def apply(): Long = {
    val instructions = read

    @tailrec
    def run(state: State): Long = {
      val next = instructions(state.instruction)(state)

      if (next.recovered.nonEmpty) {
        next.recovered.head
      } else {
        run(next.copy(
          instruction = next.instruction + 1
        ))
      }
    }

    run(State())
  }

  private def read: Vector[Instruction] = {
    (input map Instruction.read).toVector
  }

  type Register = String
  type Registers = Map[Register, Long]

  case class State(instruction: Int = 0,
                   registers: Registers = Map(),
                   sounds: List[Long] = List(),
                   recovered: List[Long] = List()
                  ) {
    def value(register: Register): Long = {
      registers.getOrElse(register, 0)
    }
  }

  sealed trait Instruction {
    def apply(state: State): State
  }

  case class Sound(register: Register) extends Instruction {
    def apply(state: State): State = state.copy(
      sounds = state.value(register) :: state.sounds
    )
  }

  case class Place(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> value(state))
    )
  }

  case class Add(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) + value(state)))
    )
  }

  case class Multiply(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) * value(state)))
    )
  }

  case class Modulus(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) % value(state)))
    )
  }

  case class Recover(register: Register) extends Instruction {
    def apply(state: State): State = {
      if (state.value(register) == 0) {
        state
      } else {
        state.copy(
          recovered = state.sounds.head :: state.recovered
        )
      }
    }
  }

  case class Jump(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = {
      if (state.value(register) <= 0) {
        state
      } else {
        state.copy(
          instruction = state.instruction + value(state).toInt - 1
        )
      }
    }
  }

  object Instruction {
    private val pattern = "(\\w+) (.+)".r
    private val subPattern = "(\\w) (.+)".r

    def read(source: String): Instruction = {
      val pattern(kind, rest) = source

      kind match {
        case "snd" => Sound(rest)
        case "set" => read(Place.apply, rest)
        case "add" => read(Add.apply, rest)
        case "mul" => read(Multiply.apply, rest)
        case "mod" => read(Modulus.apply, rest)
        case "rcv" => Recover(rest)
        case "jgz" => read(Jump.apply, rest)
      }
    }

    private def read(factory: (String, State => Long) => Instruction, source: String): Instruction = {
      val subPattern(register, valueSource) = source

      if (valueSource matches "-?\\d+") {
        val value = valueSource.toLong

        factory(register, _ => value)
      } else {
        val valueRegister = valueSource

        factory(register, state => state.value(valueRegister))
      }
    }
  }

}
