import scala.io.Source

object Day1 {
  private lazy val input: String = Source.fromResource("day1.txt").getLines.next

  def one: Int = {
    val numbers = input map (_.asDigit)
    val other = numbers.tail :+ numbers.head

    numbers
      .zip(other)
      .filter(pair => pair._1 == pair._2)
      .map(_._1)
      .sum
  }

  def two: Int = {
    val numbers = input map (_.asDigit)
    val half = numbers.size / 2
    val other = numbers.takeRight(half) ++ numbers.take(half)

    numbers
      .zip(other)
      .filter(pair => pair._1 == pair._2)
      .map(_._1)
      .sum
  }
}
