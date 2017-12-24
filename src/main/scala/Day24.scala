import scala.io.Source

object Day24 {
  def input: Iterator[String] = Source.fromResource("day24.txt").getLines

  def one: Int = {
    def build(components: Set[Set[Int]], port: Int = 0, strength: Int = 0): Int = {
      val choices = components filter (_ contains port)

      if (choices.isEmpty) {
        strength
      } else {
        (choices map (choice => {
          val next = (choice - port).headOption.getOrElse(port)

          build(components - choice, next, strength + port + next)
        })).max
      }
    }

    build(read)
  }

  def two: Int = {
    def build(components: Set[Set[Int]], port: Int = 0, strength: Int = 0, length: Int = 0): (Int, Int) = {
      val choices = components filter (_ contains port)

      if (choices.isEmpty) {
        (strength, length)
      } else {
        val bridges = choices map (choice => {
          val next = (choice - port).headOption.getOrElse(port)

          build(components - choice, next, strength + port + next, length + 1)
        })
        val maxLength = bridges.map(_._2).max

        bridges.filter(_._2 == maxLength).maxBy(_._1)
      }
    }

    val (strength, _) = build(read)

    strength
  }

  private def read: Set[Set[Int]] =
    (input map (_.split('/').map(_.toInt).toSet)).toSet
}
