import scala.annotation.tailrec
import scala.io.Source

object Day16 {
  private lazy val input: String = Source.fromResource("day16.txt").getLines.next
  private lazy val line: Vector[Program] = (0 until 16).toVector map (i => (i + 97).toChar)
  private val dances: Int = 100000000

  def one: String = {
    val moves = input.split(',').toList map Move.read
    val state = dance(moves)(State(line))

    state.line mkString ""
  }

  def two: String = {
    val moves = input.split(',').toList map Move.read
    val initial = State(line)
    val loop = seekLoop(moves, initial)
    val state = ((0 until (dances % loop)) foldLeft initial) (dance(moves))

    state.line mkString ""
  }

  private def seekLoop(moves: List[Move], initial: State): Int = {
    @tailrec
    def step(state: State, i: Int): Int = {
      val next = dance(moves)(state)

      if (next.hashCode == initial.hashCode) {
        i + 1
      } else {
        step(next, i + 1)
      }
    }

    step(initial, 0)
  }

  private def dance(moves: List[Move])(state: State, i: Int = 0): State = {
    (moves foldLeft state) ((state, move) => move(state))
  }

  type Program = Char

  sealed trait Move {
    def apply(state: State): State
  }

  object Move {
    private val pattern = "(\\w)(.+)".r

    def read(source: String): Move = {
      val pattern(kind, rest) = source

      kind match {
        case "s" => Spin.read(rest)
        case "x" => Exchange.read(rest)
        case "p" => Partner.read(rest)
      }
    }
  }

  object Spin {
    def read(source: String): Spin = {
      Spin(source.toInt)
    }
  }

  object Exchange {
    private val pattern = "(\\d+)\\/(\\d+)".r

    def read(source: String): Exchange = {
      val pattern(a, b) = source

      Exchange(a.toInt, b.toInt)
    }
  }

  object Partner {
    private val pattern = "(\\w)\\/(\\w)".r

    def read(source: String): Partner = {
      val pattern(ap, bp) = source

      Partner(ap.head, bp.head)
    }
  }

  case class Spin(amount: Int) extends Move {
    def apply(state: State): State = {
      val (first, last) = state.line splitAt (state.line.size - amount)

      State(last ++ first)
    }
  }

  case class Exchange(a: Int, b: Int) extends Move {
    def apply(state: State): State = {
      val ap = state line a
      val bp = state line b
      val line = state.line.updated(a, bp).updated(b, ap)

      state.copy(
        line,
        state.index + (ap -> b) + (bp -> a)
      )
    }
  }

  case class Partner(ap: Program, bp: Program) extends Move {
    def apply(state: State): State = {
      val a = state index ap
      val b = state index bp
      val line = state.line.updated(a, bp).updated(b, ap)

      state.copy(
        line,
        state.index + (ap -> b) + (bp -> a)
      )
    }
  }

  case class State(line: Vector[Program], index: Map[Program, Int])

  object State {
    def apply(line: Vector[Program]): State = {
      State(
        line,
        line.zipWithIndex.toMap
      )
    }
  }

}
