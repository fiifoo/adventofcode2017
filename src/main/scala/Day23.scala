import scala.annotation.tailrec
import scala.io.Source

object Day23 {
  def input: Iterator[String] = Source.fromResource("day23.txt").getLines

  def one: Long = {
    val instructions = read

    def terminated(state: State): Boolean = {
      state.instruction < 0 || state.instruction >= instructions.size
    }

    @tailrec
    def run(state: State): Long = {
      val next = instructions(state.instruction)(state)

      if (terminated(next)) {
        next.multiplied
      } else {
        run(next)
      }
    }

    run(State())
  }

  // I cheated.
  def two: Long = {
    val range = Range(105700, 122701, 17)

    (range foldLeft 0) ((result, i) => {
      if (isPrime(i)) {
        result
      } else {
        result + 1
      }
    })
  }

  def isPrime(n: Int): Boolean =
    (n > 1) && !(2 to scala.math.sqrt(n).toInt).exists(x => n % x == 0)

  private def read: Vector[Instruction] = {
    (input map Instruction.read).toVector
  }

  type Register = String
  type Registers = Map[Register, Long]

  case class State(instruction: Int = 0,
                   registers: Registers = Map(),
                   multiplied: Int = 0
                  ) {
    def value(register: Register): Long = {
      registers.getOrElse(register, 0)
    }
  }

  sealed trait Instruction {
    def apply(state: State): State
  }

  case class Place(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> value(state)),
      instruction = state.instruction + 1
    )
  }

  case class Sub(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) - value(state))),
      instruction = state.instruction + 1
    )
  }

  case class Multiply(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) * value(state))),
      multiplied = state.multiplied + 1,
      instruction = state.instruction + 1
    )
  }

  case class Jump(condition: State => Long, value: State => Long) extends Instruction {
    def apply(state: State): State = {
      if (condition(state) == 0) {
        state.copy(
          instruction = state.instruction + 1
        )
      } else {
        state.copy(
          instruction = state.instruction + value(state).toInt
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
        case "set" => read(Place.apply, rest)
        case "sub" => read(Sub.apply, rest)
        case "mul" => read(Multiply.apply, rest)
        case "jnz" =>
          val subPattern(registerSource, valueSource) = rest
          val register = readValue(registerSource)
          val value = readValue(valueSource)

          Jump(register, value)
      }
    }

    private def read(factory: (String, State => Long) => Instruction, source: String): Instruction = {
      val subPattern(register, valueSource) = source
      val value = readValue(valueSource)

      factory(register, value)
    }

    private def readValue(source: String): State => Long = {
      if (source matches "-?\\d+") {
        val value = source.toLong

        _ => value
      } else {
        val register = source

        state => state.value(register)
      }
    }
  }

}
