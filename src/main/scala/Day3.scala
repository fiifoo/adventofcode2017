import scala.annotation.tailrec

object Day3 {
  private val input: Int = 347991

  def one: Int = {
    val base = math.sqrt(input).ceil
    val side = if (base % 2 == 0) base + 1 else base
    val half = (side / 2).floor
    val x = half

    val min = math.pow(side - 2, 2) + 1
    val normalized = (input - min) % (side - 1)
    val y = math.abs(normalized - (half - 1))

    (x + y).toInt
  }

  def two: Int = {
    var data = Map(Location(0, 0) -> 1)

    @tailrec
    def seek(step: Int = 1): Int = {
      var result: Option[Int] = None

      val locations =
        ((1 - step to step) map (y => Location(step, y))) ++ // right
          ((1 - step to step) map (nx => Location(-nx, step))) ++ // up
          ((1 - step to step) map (ny => Location(-step, -ny))) ++ // left
          ((1 - step to step) map (x => Location(x, -step))) // down

      locations foreach (location => {
        val value = location(data)
        if (value > input && result.isEmpty) {
          result = Some(value)
        }

        data = data + (location -> value)
      })

      if (result.isDefined) {
        result.get
      } else {
        seek(step + 1)
      }
    }

    seek()
  }

  case class Location(x: Int, y: Int) {

    def apply(data: Map[Location, Int]): Int = {
      (adjacent flatMap data.get).sum
    }

    private def adjacent: List[Location] = {
      List(
        Location(x, y + 1),
        Location(x + 1, y + 1),
        Location(x + 1, y),
        Location(x + 1, y - 1),
        Location(x, y - 1),
        Location(x - 1, y - 1),
        Location(x - 1, y),
        Location(x - 1, y + 1)
      )
    }
  }

}
