import scala.io.Source

object Day12 {

  private val pattern = "(\\d+) <-> (.+)".r

  private def input: Iterator[String] = Source.fromResource("day12.txt").getLines

  def one: Int = {
    val data = (input map readLine).toMap

    readGroup(data)(Set(), 0).size
  }

  def two: Int = {
    val data = (input map readLine).toMap

    readGroups(data, List()).size
  }

  private def readLine(line: String): (Int, Set[Int]) = {
    val pattern(id, adjacent) = line

    (id.toInt, (adjacent split ", " map (_.toInt)).toSet)
  }

  private def readGroup(data: Map[Int, Set[Int]])
                       (result: Set[Int], id: Int): Set[Int] = {
    if (result contains id) {
      result
    } else {
      data.get(id) map (adjacent => {
        val next = result + id

        next ++ (adjacent foldLeft next) (readGroup(data))
      }) getOrElse {
        result
      }
    }
  }

  private def readGroups(data: Map[Int, Set[Int]], result: List[Set[Int]]): List[Set[Int]] = {
    data.keys.headOption map (id => {
      val group = readGroup(data)(Set(), id)

      readGroups(data -- group, group :: result)
    }) getOrElse {
      result
    }
  }
}
