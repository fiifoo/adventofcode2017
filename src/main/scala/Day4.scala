import scala.io.Source

object Day4 {
  private def input: Iterator[String] = Source.fromResource("day4.txt").getLines

  def one: Int = {
    val data = input map (_.split(" "))

    data count (words => words.toSet.size == words.length)
  }

  def two: Int = {
    val data = input map (_.split(" ") map (_.sorted))

    data count (words => words.toSet.size == words.length)
  }
}
