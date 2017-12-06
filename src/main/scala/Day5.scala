import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  private def input: Iterator[String] = Source.fromResource("day5.txt").getLines

  def one: Int = {
    val instructions = input.map(_.toInt).toArray

    @tailrec
    def step(index: Int, steps: Int): Int = {
      if (instructions.isDefinedAt(index)) {
        val value = instructions(index)
        instructions(index) = value + 1

        step(
          index + value,
          steps + 1,
        )
      } else {
        steps
      }
    }

    step(0, 0)
  }

  def two: Int = {
    val instructions = input.map(_.toInt).toArray

    @tailrec
    def step(index: Int, steps: Int): Int = {
      if (instructions.isDefinedAt(index)) {
        val value = instructions(index)
        val adjust = if (value >= 3) -1 else 1
        instructions(index) = value + adjust

        step(
          index + value,
          steps + 1,
        )
      } else {
        steps
      }
    }

    step(0, 0)
  }
}
