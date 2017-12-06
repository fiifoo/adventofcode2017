import scala.io.Source

object Day2 {
  private def input: Iterator[String] = Source.fromResource("day2.txt").getLines

  def one: Int = {
    input
      .map(_.split("\\t") map (_.toInt))
      .map(numbers => numbers.max - numbers.min)
      .sum
  }

  def two: Int = {
    val data = input map (_.split("\\t").map(_.toInt).toSet)

    (data flatMap (numbers => {
      numbers flatMap (number => numbers - number flatMap (other => {
        if (number % other == 0) {
          Some(number / other)
        } else {
          None
        }
      }))
    })).sum
  }
}
