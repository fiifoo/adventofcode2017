import scala.io.Source

object Day8 {
  def input: Iterator[String] = Source.fromResource("day8.txt").getLines

  def one: Int = {
    val (data, instructions) = read
    val calculated = (instructions foldLeft data) ((data, instruction) => instruction(data))

    calculated.values.max
  }

  def two: Int = {
    val (data, instructions) = read
    val (_, result) = (instructions foldLeft(data, 0)) ((carry, instruction) => {
      val (data, result) = carry
      val next = instruction(data)
      val value = next(instruction.target)

      (next, result max value)
    })

    result
  }

  private def read: (Map[Id, Int], List[Instruction]) = {
    val instructions = (input map Instruction.read).toList
    val data = (instructions map (_.target -> 0)).toMap

    (data, instructions)
  }

  type Id = String

  sealed trait Comparison {
    def apply(a: Int, b: Int): Boolean
  }

  object Equal extends Comparison {
    def apply(a: Int, b: Int): Boolean = a == b
  }

  object NotEqual extends Comparison {
    def apply(a: Int, b: Int): Boolean = a != b
  }

  object Less extends Comparison {
    def apply(a: Int, b: Int): Boolean = a < b
  }

  object LessOrEqual extends Comparison {
    def apply(a: Int, b: Int): Boolean = a <= b
  }

  object Greater extends Comparison {
    def apply(a: Int, b: Int): Boolean = a > b
  }

  object GreaterOrEqual extends Comparison {
    def apply(a: Int, b: Int): Boolean = a >= b
  }

  case class Condition(target: Id, comparison: Comparison, amount: Int) {
    def apply(data: Map[Id, Int]): Boolean = {
      comparison(data(target), amount)
    }
  }

  case class Instruction(target: Id, amount: Int, condition: Condition) {
    def apply(data: Map[Id, Int]): Map[Id, Int] = {
      if (condition(data)) {
        data + (target -> (data(target) + amount))
      } else {
        data
      }
    }
  }

  object Comparison {
    def read(source: String): Comparison = {
      source match {
        case "==" => Equal
        case "!=" => NotEqual
        case "<" => Less
        case "<=" => LessOrEqual
        case ">" => Greater
        case ">=" => GreaterOrEqual
      }
    }
  }

  object Condition {
    private val pattern = "(\\w+) (\\S+) (-?\\d+)".r

    def read(source: String): Condition = {
      val pattern(target, comparison, amount) = source

      Condition(
        target,
        Comparison.read(comparison),
        amount.toInt
      )
    }
  }

  object Instruction {
    private val pattern = "(\\w+) (\\w+) (-?\\d+) if (.+)".r

    def read(source: String): Instruction = {
      val pattern(target, increment, amount, condition) = source

      Instruction(
        target,
        increment match {
          case "inc" => amount.toInt
          case "dec" => -amount.toInt
        },
        Condition.read(condition)
      )
    }
  }

}
