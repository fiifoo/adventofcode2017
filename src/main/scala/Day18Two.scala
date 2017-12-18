import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day18Two {
  def input: Iterator[String] = Source.fromResource("day18.txt").getLines

  def apply(): Int = {
    val instructions = read

    def terminated(a: State, b: State): Boolean = {
      a.instruction < 0 ||
        b.instruction < 0 ||
        a.instruction >= instructions.size ||
        b.instruction >= instructions.size ||
        (a.waiting && b.waiting)
    }

    @tailrec
    def run(a: State, b: State): Int = {
      var na = instructions(a.instruction)(a)
      var nb = instructions(b.instruction)(b)

      if (na.send.isDefined) {
        nb = nb.copy(receive = nb.receive enqueue na.send.get)
        na = na.copy(send = None)
      }
      if (nb.send.isDefined) {
        na = na.copy(receive = na.receive enqueue nb.send.get)
        nb = nb.copy(send = None)
      }

      if (terminated(na, nb)) {
        nb.sent
      } else {
        run(na, nb)
      }
    }

    run(State(), State(registers = Map("p" -> 1)))
  }

  private def read: Vector[Instruction] = {
    (input map Instruction.read).toVector
  }

  type Register = String
  type Registers = Map[Register, Long]

  case class State(instruction: Int = 0,
                   registers: Registers = Map(),
                   send: Option[Long] = None,
                   receive: Queue[Long] = Queue(),
                   waiting: Boolean = false,
                   sent: Int = 0
                  ) {
    def value(register: Register): Long = {
      registers.getOrElse(register, 0)
    }
  }

  sealed trait Instruction {
    def apply(state: State): State
  }

  case class Send(value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      send = Some(value(state)),
      sent = state.sent + 1,
      instruction = state.instruction + 1
    )
  }

  case class Place(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> value(state)),
      instruction = state.instruction + 1,
    )
  }

  case class Add(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) + value(state))),
      instruction = state.instruction + 1,
    )
  }

  case class Multiply(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) * value(state))),
      instruction = state.instruction + 1,
    )
  }

  case class Modulus(register: Register, value: State => Long) extends Instruction {
    def apply(state: State): State = state.copy(
      registers = state.registers + (register -> (state.value(register) % value(state))),
      instruction = state.instruction + 1,
    )
  }

  case class Receive(register: Register) extends Instruction {
    def apply(state: State): State = {
      if (state.receive.isEmpty) {
        state.copy(
          waiting = true
        )
      } else {
        val (value, queue) = state.receive.dequeue

        state.copy(
          registers = state.registers + (register -> value),
          receive = queue,
          waiting = false,
          instruction = state.instruction + 1
        )
      }
    }
  }

  case class Jump(condition: State => Long, value: State => Long) extends Instruction {
    def apply(state: State): State = {
      if (condition(state) <= 0) {
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
        case "snd" => Send(readValue(rest))
        case "set" => read(Place.apply, rest)
        case "add" => read(Add.apply, rest)
        case "mul" => read(Multiply.apply, rest)
        case "mod" => read(Modulus.apply, rest)
        case "rcv" => Receive(rest)
        case "jgz" =>
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
