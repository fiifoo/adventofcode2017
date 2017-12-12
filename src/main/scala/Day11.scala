import scala.io.Source

object Day11 {

  private lazy val input: String = Source.fromResource("day11.txt").getLines.next

  def one: Int = {
    val directions = input split ',' map Direction.apply

    val from = Location()
    val to = (directions foldLeft from) (_ (_))

    from.distance(to)
  }

  def two: Int = {
    val directions = input split ',' map Direction.apply

    val from = Location()
    val (_, max) = (directions foldLeft(from, 0)) ((carry, direction) => {
      val (location, max) = carry
      val to = location(direction)
      val distance = from.distance(to)

      (to, math.max(max, distance))
    })

    max
  }

  case class Location(x: Int = 0, y: Int = 0, z: Int = 0) {
    def apply(direction: Direction): Location = direction(this)

    def distance(to: Location): Int = {
      math.max(
        math.max(
          math.abs(this.x - to.x),
          math.abs(this.y - to.y)
        ),
        math.abs(this.z - to.z)
      )
    }
  }

  sealed trait Direction {
    def apply(location: Location): Location
  }

  case object NorthWest extends Direction {
    def apply(location: Location): Location = Location(
      location.x - 1,
      location.y + 1,
      location.z
    )
  }

  case object North extends Direction {
    def apply(location: Location): Location = Location(
      location.x,
      location.y + 1,
      location.z - 1
    )
  }

  case object NorthEast extends Direction {
    def apply(location: Location): Location = Location(
      location.x + 1,
      location.y,
      location.z - 1
    )
  }


  case object SouthEast extends Direction {
    def apply(location: Location): Location = Location(
      location.x + 1,
      location.y - 1,
      location.z
    )
  }

  case object South extends Direction {
    def apply(location: Location): Location = Location(
      location.x,
      location.y - 1,
      location.z + 1
    )
  }

  case object SouthWest extends Direction {
    def apply(location: Location): Location = Location(
      location.x - 1,
      location.y,
      location.z + 1
    )
  }

  object Direction {
    def apply(source: String): Direction = {
      source match {
        case "nw" => NorthWest
        case "n" => North
        case "ne" => NorthEast
        case "se" => SouthEast
        case "s" => South
        case "sw" => SouthWest
      }
    }
  }

}
