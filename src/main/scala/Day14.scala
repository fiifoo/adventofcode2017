object Day14 {
  private val input: String = "wenycdww"

  def one: Int = {
    (read map (_ count (_ == true))).sum
  }

  def two: Int = {
    type Regions = Set[Set[Location]]

    case class Location(x: Int, y: Int)

    val lines = read

    def process(regions: Regions, location: Location): Regions = {
      val left = Location(location.x - 1, location.y)
      val up = Location(location.x, location.y - 1)

      var region = Set(location)
      var result = regions + region

      if (location.x > 0 && lines(left.y)(left.x)) {
        val previous = (result find (_ contains left)).get
        val next = previous ++ region

        result = result - region - previous + next
        region = next
      }

      if (location.y > 0 && lines(up.y)(up.x)) {
        val previous = (result find (_ contains up)).get
        val next = previous ++ region

        result = result - region - previous + next
        region = next
      }

      result
    }

    val initial: Regions = Set()
    val regions = (lines.zipWithIndex foldLeft initial) ((regions, item) => {
      val (line, y) = item

      (line.zipWithIndex foldLeft regions) ((regions, item) => {
        val (used, x) = item
        val location = Location(x, y)

        if (used) {
          process(regions, location)
        } else {
          regions
        }
      })
    })

    regions.size
  }

  private def read: Vector[Vector[Boolean]] = {
    (0 to 127).toVector map readLine
  }

  private def readLine(index: Int): Vector[Boolean] = {
    val hash = Day10.hash(input + "-" + index)
    val binary = BigInt(hash, 16).toString(2).reverse.padTo(128, '0').reverse

    binary.toVector map (_ == '1')
  }
}
